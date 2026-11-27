library(terra)
library(fs)
library(stringr)

# 1. 設定基礎路徑與變數清單
# 假設你的每日預測結果是存在 Temp_runs/ 目錄下的各個 run 資料夾中
runs_base_path <- here::here("Report of Foresty_20261008/heatMap/Temp_runs/") 
vars <- c("range", "encounter_rate", "count", "abundance")

# 2. 建立儲存最終整體平均結果的資料夾
output_dir <- here::here("./Report of Foresty_20261008/heatMap/aggregated/")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# 3. 使用迴圈批次處理 4 種變數
for (v in vars) {
  cat("========================================\n")
  cat("正在計算 2021-2025 總平均，變數:", v, "\n")
  cat("========================================\n")
  
  # A. 跨所有迭代（run_1 到 run_20）抓取該變數所有的 tif 檔
  all_var_tifs <- dir_ls(runs_base_path, recurse = TRUE, type = "file") %>% 
    str_subset("tif") %>% 
    str_subset(paste0("_", v, "_"))
  
  if (length(all_var_tifs) == 0) {
    cat("找不到變數", v, "的相關 TIF 檔案，跳過...\n\n")
    next
  }
  
  cat(sprintf("-> 找到 %d 個對應的預測圖層檔案，正在讀取並計算總平均...\n", length(all_var_tifs)))
  
  # B. 讀取所有檔案並組合成多圖層光柵 (SpatRaster)
  r_all <- rast(all_var_tifs)
  
  # C. 根據你的「Method B」邏輯：
  # 直接對所有 20 次重複抽樣、所有天數的圖層進行像元平均 (Pixel-wise mean)
  # 這相當於融合了所有疊代與所有時間點的最終整體平均
  overall_mean <- mean(r_all, na.rm = TRUE)
  
  # D. 輸出儲存結果
  output_filename <- file.path(output_dir, paste0("overall_average_2021_2025_", v, ".tif"))
  writeRaster(
    overall_mean, 
    output_filename, 
    overwrite = TRUE, 
    gdal = c("COMPRESS=LZW")
  )
  
  cat(sprintf("-> [完成] %s 的 2021-2025 總平均已存檔至: %s\n\n", v, output_filename))
}

cat("所有變數的 2021-2025 總平均已經全部計算完畢！\n")
