#---- load library
library(readxl)
library(writexl)
library(tidyverse)
library(sf)

#----------------
#read forest spatial data, merge polgons by TypeName, area by TypeName

path <- "D:/R/test/第四次森林資源調查全島森林林型分布圖"

nc <- st_read(paste0(path,"/","全島森林林型分布圖.shp"),
              crs="+init=epsg:3826") %>% 
  arrange(TypeName, st_geometry_type(geometry), FunctionTy,Function)

nc.a<-
  nc$TypeName %>% 
  as.character %>%
  unique %>% 
  lapply(.,
         function(x){
           tmp <- 
             nc %>%
             filter(TypeName %in% x) %>% 
             st_combine() 
           
           st_sf(TypeName= x, geom = st_sfc(tmp), crs = 3826)
         }) %>% 
  do.call(rbind,.)


bb <- 
  nc %>%
  st_drop_geometry %>% 
  group_by(TypeName) %>%
  summarise(area = sum(Area_ha) %>% round(.,4))

nc.b <- 
  left_join(nc.a, bb) %>% 
  filter(!TypeName %in% c("待成林地", "裸露地", "陰影")) %>% 
  mutate(TypeName.1 = ifelse(TypeName %in% "闊葉樹林型", "闊葉林",
                         ifelse(TypeName %in% "針葉樹林型", "針葉林",
                                ifelse(TypeName %in% "竹林", "竹林", "混淆林"))))


#-----------------------------------------------------
#county
path2 <- "D:/R/test/COUNTY_MOI_1081121"

TW <- st_read(paste0(path2,"/","COUNTY_MOI_1081121.shp")) %>% 
  filter(!COUNTYNAME %in% c("連江縣", "澎湖縣","金門縣")  )
st_transform(TW,4326)




#---------



ggplot()+
  geom_sf(data = nc.b, aes(fill = TypeName.1, color = TypeName.1))


ggplot()+
  geom_sf(data = TW, alpha = 0)+
  geom_sf(data = nc.b, aes(fill = TypeName.1, color = TypeName.1), alpha = 0.2)+
  
  geom_sf(data = TW, alpha = 0)+
  coord_sf(xlim = c(119.5, 122.5), ylim = c(21.5, 25.5), expand = FALSE)
  

#-------------
library(raster)


str_name<-'D:/R/test/不分幅_全台及澎湖/dem_20m.tif' 
imported_raster=raster(str_name)


imported_raster@crs

contour(imported_raster, add=TRUE)
filledContour(imported_raster)

x <- rasterToContour(imported_raster)
x1 <- st_as_sf(x)
st_transform(x1,4326)

plot(x1)

s <- stack(imported_raster)


plot(imported_raster)


fun <- function(x) { x[x<50] <- NA; return(x) }

Sys.time()
rc2 <- calc(imported_raster, fun) 

Sys.time()
rc3 <- rc2%>% 
  rasterToPolygons(.) %>% 
  st_combine %>% 
  st_as_sf(.)
Sys.time()

#-----------------------
#Reference
#https://geocompr.robinlovelace.net/geometric-operations.html