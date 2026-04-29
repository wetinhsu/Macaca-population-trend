
library(tidyverse)
library(sf)
library(DBI)
library(readxl)
#--------------------------------------------------------------------
#read forest spatial data

path1 <- "D:/R/SHP圖層/0_自製/POLYGON_250"
path2 <- "D:/R/SHP圖層/0_自製/POLYGON_500"
path3 <- "D:/R/SHP圖層/0_自製/POLYGON_1000"
path4 <- "D:/R/SHP圖層/0_自製/POLYGON_50"

POLYGON_50 <- st_read(paste0(path4,"/","POLYGON_50.shp"), crs=3826)
POLYGON_250 <- st_read(paste0(path1,"/","POLYGON_250.shp"), crs=3826)
POLYGON_500 <- st_read(paste0(path2,"/","POLYGON_500.shp"), crs=3826)
POLYGON_1000 <- st_read(paste0(path3,"/","POLYGON_1000.shp"), crs=3826)


troops<- read_xlsx(
  here("合併估算_20260424/troops_2021-2024.xlsx")) 

con <-  dbConnect(RSQLite::SQLite(), dbname="D:/R/test/DB/P_BBS.db")
list_Point<-
  dbReadTable(con, "list_Point") %>% 
  arrange(樣區編號, 獼猴樣區編號,as.numeric(樣點代號)) 
dbDisconnect(con)

up_50 <- 
list_Point %>% 
  st_as_sf(.,coords = c('X_wgs84',  'Y_wgs84'), crs = 4326) %>% 
  st_transform(3826) %>% 
  st_filter(POLYGON_50)

poly_diff_250 <- st_difference(POLYGON_50, POLYGON_250)
poly_diff_500 <- st_difference(POLYGON_50, POLYGON_500)
poly_diff_1000 <- st_difference(POLYGON_50, POLYGON_1000)

until_250 <- up_50%>% 
  st_filter(poly_diff_250) %>% 
  inner_join(troops, ., by = c('樣區編號', '獼猴樣區編號', 'Point'="樣點代號")) 


until_500 <- up_50%>% 
  st_filter(poly_diff_500) %>% 
  inner_join(troops, ., by = c('樣區編號', '獼猴樣區編號', 'Point'="樣點代號")) 

until_1000 <- up_50%>% 
  st_filter(poly_diff_1000) %>% 
  inner_join(troops, ., by = c('樣區編號', '獼猴樣區編號', 'Point'="樣點代號")) 







st_write(until_250, "合併估算_20260424/bird_points.sqlite", driver = "SQLite", layer = "until_250")
st_write(until_500, "合併估算_20260424/bird_points.sqlite", driver = "SQLite", layer = "until_500")
st_write(until_1000, "合併估算_20260424/bird_points.sqlite", driver = "SQLite", layer = "until_1000")

