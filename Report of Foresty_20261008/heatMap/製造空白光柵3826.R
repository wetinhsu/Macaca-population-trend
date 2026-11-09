library(terra)

# 1. 讀取原本有海拔資料的 tif 檔
r_elevation <- rast("D:/R/SHP圖層/Taiwan_environmental_dataset-master/GeoTIFF/G1km_TWD97-121_DTM_/G1km_TWD97-121_DTM_ELE.tif")

# 2. 複製一份結構完全相同的光柵，並把所有格子的數值設為 NA (空資料)
r_empty <- r_elevation
values(r_empty) <- NA
names(r_empty) <- "layer"

# 3. 將這個空白模板存成新的 tif 檔
writeRaster(r_empty, "Report of Foresty_20261008/heatMap/empty_grid_3826.tif", overwrite = TRUE, gdal = c("COMPRESS=LZW"))

cat("空白的 1x1km 網格模板已成功儲存！\n")