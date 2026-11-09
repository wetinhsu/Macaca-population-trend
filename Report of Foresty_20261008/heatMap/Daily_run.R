library(parallel)
library(terra)
library(dplyr)
library(tidyr)
library(lubridate)
library(ranger)

# 1. 建立儲存 TIF 檔的資料夾
dir.create("Report of Foresty_20261008/heatMap/Daily", showWarnings = FALSE, recursive = TRUE)

# 2. 設定你要跑的總日期範圍
target_dates_all <- c(seq(ymd("2021-03-01"), ymd("2021-06-30"), by = "days"),
                      seq(ymd("2022-03-01"), ymd("2022-06-30"), by = "days"),
                      seq(ymd("2023-03-01"), ymd("2023-06-30"), by = "days"),
                      seq(ymd("2024-03-01"), ymd("2024-06-30"), by = "days"),
                      seq(ymd("2025-03-01"), ymd("2025-06-30"), by = "days"))

# 3. 依照「年-月」自動將日期分組
date_groups <- split(target_dates_all, format(target_dates_all, "%Y-%m"))

# 4. 建立平行運算叢集 (Cluster)
num_cores <- max(1, detectCores() - 1)
cl <- makeCluster(num_cores)

# 5. 將必要套件載入到每個背景核心中
clusterEvalQ(cl, {
  library(dplyr)
  library(ranger)
  library(lubridate)
})

# 6. ️ 只需要匯出隨機森林模型與預測網格（不要匯出 calibration_model！）
clusterExport(cl, varlist = c(
  "pred_grid", 
  "rf_range", 
  "rf_er", 
  "rf_count"
))

vars <- c("range", "encounter_rate", "count", "abundance")

# 7. 開始逐月迴圈處理
for (m_name in names(date_groups)) {
  cat("========================================\n")
  cat("正在處理月份:", m_name, "\n")
  cat("========================================\n")
  
  days_in_month <- date_groups[[m_name]]
  n_days <- length(days_in_month)
  

  
  # A. 使用 parLapply 平行計算該月每一天的「原始」預測
  pred_results_list <- parLapply(cl, days_in_month, function(d) {
    d <- as_date(d)
    
    # 設定當天的標準化努力值
    pred_grid_eff <- pred_grid |>
      mutate(
        observation_date = d,
        year = year(observation_date),
        day_of_year = yday(observation_date),
        effort_n = 6,
        number_n = 1
      )
    
    # 執行模型預測 (num.threads = 2 避免背景核心內部資源衝突)
    pred_range <- as.integer(
      predict(rf_range, data = pred_grid_eff, type = "response", num.threads = 2)$predictions == "TRUE"
    )
    
    # 取得未校準的原始遭遇率
    pred_er <- predict(rf_er, data = pred_grid_eff, type = "response", num.threads = 2)$predictions[, "TRUE"]
    
    pred_grid_eff$predicted_er <- pred_er
    pred_count <- predict(rf_count, data = pred_grid_eff, type = "response", num.threads = 2)$predictions
    
    # 僅回傳原始預測結果（不在此處做校準）
    data.frame(
      cell_id = pred_grid_eff$cell,
      date    = d,
      range   = pred_range,
      pred_er = pred_er,
      count   = pred_count
    )
  })
  

  
  # B. 合併該月所有天數的資料
  month_preds_df <- bind_rows(pred_results_list)
  
  # C.  在「主行程」進行校準與計算相對豐度（安全、不會出錯）
  month_preds_df <- month_preds_df %>%
    mutate(
      encounter_rate = predict(calibration_model, data.frame(pred = pred_er), type = "response") |> as.numeric(),
      encounter_rate = pmax(0, pmin(1, encounter_rate)), # 限制在 0-1 之間
      abundance      = range * encounter_rate * count
    )
  
  date_strs <- as.character(days_in_month)
  
  # D. 針對每一個變數建立當月的時序光柵並直接匯出存檔
  for (v in vars) {
    df_wide <- month_preds_df %>%
      select(cell_id, date, all_of(v)) %>%
      pivot_wider(names_from = date, values_from = all_of(v))
    
    r_temp <- rast(r, nlyrs = n_days)
    names(r_temp) <- date_strs
    r_temp[df_wide$cell_id] <- as.matrix(df_wide[, -1])
    
    # 存檔至 monthly 資料夾
    filename <- paste0("Report of Foresty_20261008/heatMap/Daily/stack_", v, "_", m_name, ".tif")
    writeRaster(r_temp, filename, overwrite = TRUE, gdal = c("COMPRESS=LZW"))
  }
  
  cat("月份", m_name, "處理完畢並已存檔！\n\n")
}

# 8. 關閉叢集，釋放記憶體與 CPU 資源
stopCluster(cl)

