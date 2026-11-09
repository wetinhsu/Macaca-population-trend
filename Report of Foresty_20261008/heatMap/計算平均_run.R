library(terra)
library(fs)
library(stringr)

# 1. 設定基礎路徑與變數清單
path <- here::here("./Report of Foresty_20261008/heatMap/Daily/")
vars <- c("range", "encounter_rate", "count", "abundance")

# 2. 建立儲存平均結果的資料夾
dir.create("Report of Foresty_20261008/heatMap/aggregated", showWarnings = FALSE, recursive = TRUE)

# 3. 使用迴圈批次處理 4 種變數
for (v in vars) {
  cat("========================================\n")
  cat("正在處理變數:", v, "\n")
  cat("========================================\n")
  
  # 抓取該變數對應的所有月度 tif 檔
  monthly_heat_ls <- dir_ls(path, recurse = TRUE, type = "file") %>% 
    str_subset("tif") %>% 
    str_subset(paste0("_", v, "_"))
  
  if (length(monthly_heat_ls) == 0) {
    cat("找不到變數", v, "的相關檔案，跳過...\n\n")
    next
  }
  
  # 將該變數的所有 tif 結合成一個多圖層光柵
  r_all <- rast(monthly_heat_ls)
  
  # 解析圖層名稱中的日期
  dates <- names(r_all) %>%
    str_remove("^X") %>%             
    str_replace_all("\\.", "-") %>%    
    as.Date()                        
  
  # A. 計算並儲存【月平均】 (Monthly Average)
  month_index <- format(dates, "%Y-%m")
  monthly_stack <- tapp(r_all, index = month_index, fun = mean, na.rm = TRUE)
  writeRaster(monthly_stack, paste0("Report of Foresty_20261008/heatMap/aggregated/monthly_average_", v, ".tif"), 
              overwrite = TRUE, gdal = c("COMPRESS=LZW"))
  cat("-> 月平均已儲存\n")
  
  # B. 計算並儲存【年平均】 (Yearly Average)
  year_index <- format(dates, "%Y")
  yearly_stack <- tapp(r_all, index = year_index, fun = mean, na.rm = TRUE)
  writeRaster(yearly_stack, paste0("Report of Foresty_20261008/heatMap/aggregated/yearly_average_", v, ".tif"), 
              overwrite = TRUE, gdal = c("COMPRESS=LZW"))
  cat("-> 年平均已儲存\n")
  
  # C. 計算並儲存【2021-2025 跨年總平均】 (Overall Average)
  # 直接對所有圖層計算像元平均，產出單層光柵
  overall_mean <- mean(r_all, na.rm = TRUE)
  writeRaster(overall_mean, paste0("Report of Foresty_20261008/heatMap/aggregated/overall_average_2021_2025_", v, ".tif"), 
              overwrite = TRUE, gdal = c("COMPRESS=LZW"))
  cat("-> 2021-2025 總平均已儲存\n\n")
}

cat("所有變數的月平均、年平均與 2021-2025 總平均已經全部計算並存檔完畢！\n")
