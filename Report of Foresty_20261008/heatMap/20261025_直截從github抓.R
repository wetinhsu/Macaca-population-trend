
# 載入需要的套件 (若未安裝請先用 install.packages 安裝)
library(jsonlite)
library(terra)

# ==========================================
# 1. 設定你的 GitHub 專案資訊
# ==========================================
api_url <- "https://api.github.com/repos/WanJyunChen/Taiwan_environmental_dataset/contents/GeoTIFF"

# ==========================================
# 2. 自動抓取 API 清單並篩選出所有的 .zip 檔案
# ==========================================
cat("正在取得 GitHub 上的檔案清單...\n")
file_info <- fromJSON(api_url)

# 過濾出副檔名為 .zip 的檔案
zip_files <- file_info[grepl("\\.zip$", file_info$name, ignore.case = TRUE), ]

if (nrow(zip_files) == 0) {
  stop("找不到任何 .zip 檔案，請檢查路徑是否正確！")
}

cat(paste("找到", nrow(zip_files), "個壓縮檔，準備開始下載與解壓縮...\n"))

# ==========================================
# 3. 建立暫存區並批次下載、解壓縮
# ==========================================
temp_dir <- tempdir()

for (i in 1:nrow(zip_files)) {
  zip_name <- zip_files$name[i]
  zip_url  <- zip_files$download_url[i]
  dest_zip <- file.path(temp_dir, zip_name)
  
  cat(sprintf("[%d/%d] 正在下載: %s\n", i, nrow(zip_files), zip_name))
  download.file(zip_url, dest_zip, mode = "wb", quiet = TRUE)
  
  # 解壓縮到暫存區
  unzip(dest_zip, exdir = temp_dir)
}

# ==========================================
# 4. 搜集所有解壓縮出來的 .tif 檔並讀取
# ==========================================
cat("正在搜尋並載入所有的 .tif 檔案...\n")
tif_files <- list.files(temp_dir, pattern = "121.*\\.tif$", full.names = TRUE, recursive = TRUE)

cat(paste("總共找到", length(tif_files), "個 .tif 檔案。\n"))

# 用 terra 批次讀取 (形成 SpatRaster 物件)
r_stack <- rast(tif_files)

# ==========================================
# 5. 完成！顯示圖資基本資訊
# ==========================================
print("處理完成！圖資結構如下：")
print(r_stack)

# 範例：如果你想畫圖檢查
# plot(r_stack)