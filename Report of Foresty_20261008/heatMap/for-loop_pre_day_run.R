
# 設定你想計算的月份（例如 2024 年 4 月）
target_dates <- seq(ymd("2024-03-01"), ymd("2024-06-30"), by = "days")


# 1. 建立一個空的 List 用來存放每一天的預測資料框
pred_results_list <- list()

#事先準備一個單層的空白光柵範本（提供相同的空間範圍與網格結構）
r_template <- rast(r)

for (d in target_dates) {
  d <- as_date(d)

  # A. 設定當天的標準化努力值
  pred_grid_eff <- pred_grid |>
    mutate(observation_date = d,
           year = year(observation_date),
           day_of_year = yday(observation_date),
           effort_n = 6,
           number_n = 1)

  # B. 執行模型預測 (range, er, count, abundance)
  pred_range <- as.integer(predict(rf_range, data = pred_grid_eff, type = "response")$predictions == "TRUE")

  pred_er <- predict(rf_er, data = pred_grid_eff, type = "response")$predictions[, "TRUE"]

  pred_er_cal <- predict(calibration_model, data.frame(pred = pred_er), type = "response") |> as.numeric()
  pred_er_cal[pred_er_cal < 0] <- 0
  pred_er_cal[pred_er_cal > 1] <- 1

  pred_grid_eff$predicted_er <- pred_er
  pred_count <- predict(rf_count, data = pred_grid_eff, type = "response")$predictions

  # 計算該日期的相對豐度
  pred_abundance <- pred_range * pred_er_cal * pred_count

  # C. 把當天的結果包成資料框，並加上日期標籤
  df_day <- data.frame(
    cell_id        = pred_grid_eff$cell,
    date           = d,
    range          = pred_range,
    encounter_rate = pred_er_cal,
    count          = pred_count,
    abundance      = pred_range * pred_er_cal * pred_count
  )

  # D. 將當天資料框放入 list 中
  pred_results_list[[as.character(d)]] <- df_day
}

# ==========================================
# 迴圈結束後：合併所有日期的資料，並轉換為時序光柵
# ==========================================

# 1. 把所有日期的 data.frame 合併成一個超長表格
all_preds_df <- bind_rows(pred_results_list)

# 2. 建立一個 30 圖層的空白光柵底圖（對應 30 個日期）
n_days <- length(target_dates)
r_template_30 <- rast(r, nlyrs = n_days)
names(r_template_30) <- as.character(target_dates)

# 3. 利用 pivot_wider 將長表格轉成「每一欄代表一天」的矩陣格式，再批次填入光柵
# 以 range 為例：
df_range_wide <- all_preds_df %>%
  select(cell_id, date, range) %>%
  pivot_wider(names_from = date, values_from = range)

# 抽出數值矩陣（排除掉第一欄 cell_id）
matrix_range <- as.matrix(df_range_wide[, -1])

# 一次性將 30 天的 range 填入 30 圖層光柵中！
r_template_30[df_range_wide$cell_id] <- matrix_range
stack_range <- r_template_30

# 檢查結果
print(stack_range) # 應該會看到 30 個圖層，圖層名稱就是日期


#--------------------
library(terra)
library(dplyr)
library(tidyr)

# 1. 基本設定
n_days <- length(target_dates)
date_strs <- as.character(target_dates)
all_preds_df <- bind_rows(pred_results_list)

# 2. 定義要轉換的變數名稱
vars <- c("range", "encounter_rate", "count", "abundance")

# 3. 用迴圈批次轉成時序光柵並存入 list
raster_stacks <- list()

for (v in vars) {
  # 寬表格轉換
  df_wide <- all_preds_df %>%
    select(cell_id, date, all_of(v)) %>%
    pivot_wider(names_from = date, values_from = all_of(v))

  # 建立空白時序光柵並賦值
  r_temp <- rast(r, nlyrs = n_days)
  names(r_temp) <- date_strs
  r_temp[df_wide$cell_id] <- as.matrix(df_wide[, -1])

  # 存入結果清單
  raster_stacks[[v]] <- r_temp
}

# 4. 賦值給各自獨立的變數名稱（方便後續直接呼叫）
stack_range     <- raster_stacks[["range"]]
stack_er        <- raster_stacks[["encounter_rate"]]
stack_count     <- raster_stacks[["count"]]
stack_abundance <- raster_stacks[["abundance"]]

# 5. (選擇性) 批次匯出成壓縮的 GeoTIFF 檔案
for (v in vars) {
  filename <- paste0("stack_", v, ".tif")
  writeRaster(raster_stacks[[v]], filename, overwrite = TRUE, gdal = c("COMPRESS=LZW"))
}

#----------------------------

dates <- as.Date(names(stack_range))
# 1. 將原本的圖層名稱（字串）轉成真正的 Date 物件
dates <- as.Date(names(stack_range))

# 2. 萃取出「年-月」作為分組索引（例如 "2024-03", "2024-04"）
month_index <- format(dates, "%Y-%m")

# 3. 使用 tapp 批次計算每個月的平均值
monthly_stack_range <- tapp(stack_range, index = month_index, fun = mean, na.rm = TRUE)

# 檢查結果（圖層名稱會變成對應的年份-月份）
print(monthly_stack_range)


plot(monthly_stack_range)
