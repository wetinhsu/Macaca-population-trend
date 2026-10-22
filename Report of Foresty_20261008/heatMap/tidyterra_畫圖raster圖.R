library(tidyterra)

ggplot() +
  geom_spatraster(data = r_pred[["abundance"]]) +
  coord_sf() +                                # 確保坐標系正確
  theme_classic()


ggplot() +
  geom_spatraster(data =stack_range[['2024-03-06']]) +
  coord_sf(crs = 4326) +                                # 確保坐標系正確
  theme_classic()

