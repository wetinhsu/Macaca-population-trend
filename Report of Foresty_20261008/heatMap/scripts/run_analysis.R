

# 1. 載入各模組
source("Report of Foresty_20261008/heatMap/scripts/01_setup.R")
source("Report of Foresty_20261008/heatMap/scripts/02_model_functions.R")
source("Report of Foresty_20261008/heatMap/scripts/03_prediction_loop.R")

n_iterations <- 20

# 2. 執行 20 次模型訓練與預測（每次跑完自動存入硬碟暫存區）
for (i in 1:n_iterations) {
  cat(sprintf("========================================\n"))
  cat(sprintf("開始執行第 %d / %d 次重複抽樣與模型訓練\n", i, n_iterations))
  cat(sprintf("========================================\n"))
  
  models <- run_model_pipeline(data_source = combine_bb, seed_val = 100 + i)
  
  run_and_save_prediction(
    models = models, 
    pred_grid = pred_grid, 
    base_raster = r, 
    run_id = i
  )
}

cat("\n========================================\n")
cat("20 次模擬全部完成！開始計算 2021-2025 Overall 總平均...\n")
cat("========================================\n")

output_dir_final <- "Report of Foresty_20261008/heatMap/Averaged_Overall"
dir.create(output_dir_final, showWarnings = FALSE, recursive = TRUE)

vars <- c("range", "encounter_rate", "count", "abundance")

# 自動偵測有哪些月份的檔案被產出過
sample_files <- list.files("Report of Foresty_20261008/heatMap/Temp_runs/run_1", pattern = "*.tif")
months_list <- unique(sub(".*_([0-9]{4}-[0-9]{2})\\.tif$", "\\1", sample_files))

# 3. 針對每個變數，計算 2021-2025 的 Overall 平均
for (v in vars) {
  
  # 收集 20 次迴圈中，該變數所有月份的總平均清單
  single_run_overall_means <- list()
  
  for (i in 1:n_iterations) {
    # 讀取該次迴圈所有月份的 TIF 檔案並合併成一個大 Stack
    tif_paths_i <- sapply(months_list, function(m_name) {
      paste0("Report of Foresty_20261008/heatMap/Temp_runs/run_", i, "/stack_", v, "_", m_name, ".tif")
    })
    
    r_stack_i <- rast(tif_paths_i)
    
    # 計算「該次抽樣」在 2021-2025 全部時間點的 Overall 平均圖層
    single_run_overall_means[[i]] <- mean(r_stack_i, na.rm = TRUE)
  }
  
  # 將 20 次各自算好的 Overall 平均圖組合起來
  r_20_runs_stack <- rast(single_run_overall_means)
  
  # 計算 20 次的最終 Overall 平均地圖
  r_final_overall_mean <- mean(r_20_runs_stack, na.rm = TRUE)
  
  # 輸出最終 Overall 平均 TIF
  out_name <- paste0(output_dir_final, "/Overall_Mean_", v, "_2021-2025.tif")
  writeRaster(r_final_overall_mean, out_name, overwrite = TRUE, gdal = c("COMPRESS=LZW"))
  
  cat("變數", v, "的 2021-2025 20次總平均地圖已順利產出！\n")
}

cat("🎉 全部流程圓滿完成！Overall 最終平均結果已儲存至:", output_dir_final, "\n")