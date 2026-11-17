library(sf)
library(tidyverse)
library(fs)
library(terra)


# 1. 讀取 5km 方格 Shapefile
grid_shp <- vect("D:/R/SHP圖層/grid_system_tw_terrestrial_v20180511/g_3826_0_2200000_s/g5km_3826_0_2200000_s.shp")

# 產生流水號 ID（1, 2, 3... 總格數）
grid_shp$id <- 1:nrow(grid_shp)

# 2. 建立一個空的目標光柵模板 (Resolution 設為 5000 公尺)
# 確保你的 shp 座標系統是公尺單位 (如 UTM 或 TWD97 TM2)
r_template <- rast(grid_shp, resolution = 5000)

# 3. 將向量轉為光柵 (field 帶入你想轉成像素值的欄位名稱)
r_rasterized <- rasterize(grid_shp, r_template, field = "id")

# 4. 檢視結果
plot(r_rasterized)

# 5. 匯出成 GeoTIFF 檔案
writeRaster(r_rasterized, "./Report of Foresty_20261008/heatMap/empty tiff/empty_grid_5km.tif", overwrite = TRUE)
