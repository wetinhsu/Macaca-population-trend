
# library(tidyverse)
# library(sf)
library(raster)

str_name<-'D:/R/test/不分幅_全台及澎湖/dem_20m.tif' 
imported_raster=raster(str_name)

s <- stack(imported_raster)



# 
# 
# S.all<- read_xlsx( "./研討會_202307/data/clean/Distance500_1522.xlsx") %>% 
#   filter(! is.na(Month)| ! is.na(Day)| ! is.na(Hour)| ! is.na(Minute)) 
# 
# S.all %>%  #確認每一旅次內的每一個樣點資料只有1筆
#   group_by(., Year, Survey, Site_N, Point) %>% 
#   summarise( n=n()) %>% 
#   filter(n >1) %>% View
# 


# 
# S.all_alt <- S.all %>%
#   filter(!is.na(X_97))  %>% 
#   mutate_at(c("X_97", "Y_97"),as.numeric) %>% 
#   st_as_sf(., coords = c("X_97", "Y_97"), remove = F, crs = 3826) %>% 
#   st_transform(crs(imported_raster)) %>% 
#   mutate(Altitude = extract(imported_raster, ., method='simple')) %>% 
#   st_drop_geometry()
#   
# 
# 
# write_xlsx(S.all_alt, "./研討會_202307/data/clean/full_combind_data_1522.xlsx")
