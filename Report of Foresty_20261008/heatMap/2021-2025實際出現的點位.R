ggplot() +
  geom_sf(data = countries, fill = "#cfcfcf", color = "#888888", linewidth = 0.25) +
  geom_sf(data = checklists_sf[checklists_sf$species_observed == TRUE,])+
  coord_sf(
    xlim = c(119.3, 122.2),  # 台灣本島大約的經度範圍
    ylim = c(21.9, 25.4),    # 台灣本島大約的緯度範圍
    expand = FALSE
  ) 


print(checklists_sf)

v <- vect(checklists_sf[checklists_sf$species_observed == TRUE,])
v_proj <- project(v, crs(r))
checklists_sp <- rasterize(v_proj, r, fun = "sum", background = 0)

# 1. 建立大於 0 為 1、其餘為 0 的二元光柵
presence_layer <- ifel(checklists_sp > 0, 1, 0)

# 2. 將這個新圖層加到原本的 checklists_sp 裡（變成 2 個圖層的 SpatRaster）
checklists_sp <- c(checklists_sp, presence_layer)

# 3. （選用）為圖層重新命名，方便後續辨識
names(checklists_sp) <- c("count", "presence")

# 檢查結果
plot(checklists_sp)

checklists_sp <- subset(checklists_sp, "presence")
plot(checklists_sp)

ggplot() +
  geom_spatraster(data = checklists_sp[["presence"]]) +

  coord_sf(  ) +                                # 確保坐標系正確
  theme_classic()

writeRaster(checklists_sp, paste0("Report of Foresty_20261008/heatMap/overall_presence_2021_2025.tif"), 
            overwrite = TRUE, gdal = c("COMPRESS=LZW"))
