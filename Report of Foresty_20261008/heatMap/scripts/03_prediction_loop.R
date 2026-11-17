library(parallel)
library(terra)
library(dplyr)
library(tidyr)
library(lubridate)
library(ranger)

run_and_save_prediction <- function(models, pred_grid, base_raster, run_id) {
  
  # 從傳入的 models 清單中取出模型
  rf_range <- models$rf_range
  rf_er <- models$rf_er
  calibration_model <- models$calibration_model
  rf_count <- models$rf_count
  
  # 建立該次專屬的暫存輸出資料夾
  output_dir <- paste0("Report of Foresty_20261008/heatMap/Temp_runs/run_", run_id)
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  target_dates_all <- c(
    seq(ymd("2021-03-01"), ymd("2021-06-30"), by = "days"),
    seq(ymd("2022-03-01"), ymd("2022-06-30"), by = "days"),
    seq(ymd("2023-03-01"), ymd("2023-06-30"), by = "days"),
    seq(ymd("2024-03-01"), ymd("2024-06-30"), by = "days"),
    seq(ymd("2025-03-01"), ymd("2025-06-30"), by = "days")
  )
  
  date_groups <- split(target_dates_all, format(target_dates_all, "%Y-%m"))
  
  num_cores <- max(1, detectCores() - 1)
  cl <- makeCluster(num_cores)
  
  clusterEvalQ(cl, {
    library(dplyr)
    library(ranger)
    library(lubridate)
  })
  
  # 確保將模型與預grid正確匯出給背景核心
  clusterExport(cl, varlist = c("pred_grid", "rf_range", "rf_er", "rf_count"), envir = environment())
  vars <- c("range", "encounter_rate", "count", "abundance")
  
  for (m_name in names(date_groups)) {
    cat(sprintf("  -> [第 %d 次迴圈] 正在處理月份: %s\n", run_id, m_name))
    days_in_month <- date_groups[[m_name]]
    n_days <- length(days_in_month)
    
    pred_results_list <- parLapply(cl, days_in_month, function(d) {
      d <- as_date(d)
      pred_grid_eff <- pred_grid |>
        mutate(
          observation_date = d,
          year = year(observation_date),
          day_of_year = yday(observation_date),
          effort_n = 6,
          number_n = 1
        )
      
      pred_range <- as.integer(
        predict(rf_range, data = pred_grid_eff, type = "response", num.threads = 2)$predictions == "TRUE"
      )
      pred_er <- predict(rf_er, data = pred_grid_eff, type = "response", num.threads = 2)$predictions[, "TRUE"]
      
      pred_grid_eff$predicted_er <- pred_er
      pred_count <- predict(rf_count, data = pred_grid_eff, type = "response", num.threads = 2)$predictions
      
      data.frame(
        cell_id = pred_grid_eff$cell,
        date    = d,
        range   = pred_range,
        pred_er = pred_er,
        count   = pred_count
      )
    })
    
    month_preds_df <- bind_rows(pred_results_list) %>%
      mutate(
        encounter_rate = predict(calibration_model, data.frame(pred = pred_er), type = "response") |> as.numeric(),
        encounter_rate = pmax(0, pmin(1, encounter_rate)),
        abundance      = range * encounter_rate * count
      )
    
    date_strs <- as.character(days_in_month)
    
    # 每次跑完立刻存成 TIF 檔，釋放記憶體
    for (v in vars) {
      df_wide <- month_preds_df %>%
        select(cell_id, date, all_of(v)) %>%
        pivot_wider(names_from = date, values_from = all_of(v))
      
      r_temp <- rast(base_raster, nlyrs = n_days)
      names(r_temp) <- date_strs
      r_temp[df_wide$cell_id] <- as.matrix(df_wide[, -1])
      
      filename <- paste0(output_dir, "/stack_", v, "_", m_name, ".tif")
      writeRaster(r_temp, filename, overwrite = TRUE, gdal = c("COMPRESS=LZW"))
    }
  }
  
  stopCluster(cl)
}