
library(dplyr)

library(ggplot2)
library(glue)
library(gridExtra)
library(lubridate)

library(scam)
library(sf)
library(terra)
library(tidyr)
library(tidyterra)
library(readxl)
library(here)
library(stringr)
library(DBI)

# 1. 讀取空間與環境網格
r <- rast("Report of Foresty_20261008/heatMap/empty_grid_3826.tif") 
r5km <- rast("Report of Foresty_20261008/heatMap/empty tiff/empty_grid_5km.tif")

# 讀取樣點資料
con <- dbConnect(RSQLite::SQLite(), dbname="D:/R/test/DB/P_BBS.db")
list_Point <- dbReadTable(con, "list_Point") %>% arrange(樣區編號, 獼猴樣區編號, as.numeric(樣點代號))
list_Site  <- dbReadTable(con, "list_Site")
table_point <- dbReadTable(con, "table_point")
dbDisconnect(con)

list_Point_sf <- list_Point %>% 
  filter(!is.na(X_97)) %>% 
  select(樣區編號, 獼猴樣區編號, 樣點代號, X_97, Y_97)

# 2. 讀取 BBS 資料
M.data <- read_excel(here("./data/clean/for analysis_1525.xlsx"), sheet=1, col_types = "text") %>% 
  filter(analysis %in% "Y") %>%
  right_join(list_Point_sf, ., by=c("樣區編號" = "Site_N", "樣點代號" = "Point"), suffix = c("", ".y")) %>% 
  mutate(julian.D = as.numeric(julian.D)) %>% 
  select(樣區編號:`Minute`, -Time, 'day_of_year' = julian.D)

# 3. 讀取林業署資料
M.data_F <- here("./data/clean/Forestry/for analysis/") %>% 
  list.files(., full.names = T) %>% 
  str_subset(paste0(2020:2025, collapse = "|")) %>% 
  lapply(., read_excel, sheet="Data", col_types = "text") %>% 
  bind_rows() %>% 
  mutate_at(c("Year", "Survey", "Month", "Day", "Macaca_sur", "Distance"), as.character) %>% 
  filter(analysis %in% "Y") %>% 
  mutate(Site_N = str_replace_all(Site_N, "MB-C11-10", "MA-C11-10")) %>% 
  right_join(list_Point_sf, ., by=c("獼猴樣區編號" = "Site_N", "樣點代號" = "Point"), suffix = c("", ".y"))  %>% 
  mutate(julian.D = as.numeric(julian.D)) %>% 
  select(樣區編號:`Y_97`, `Year`:`Macaca_dist`, -Surveyor, 'day_of_year' = julian.D)

# 4. 載入環境變數
source("D:/R/test/Macaca-population-trend/Report of Foresty_20261008/heatMap/environment.R")

environment_r <- project(environment_sp, r) %>% 
  resample(., r, method = "bilinear")

names(environment_r) <- str_remove_all(names(environment_r), ".*_")
environment_r_cell <- as.data.frame(environment_r, cells = TRUE, na.rm = TRUE)

# 5. 合併 BBS 與林業署資料
combine_bb <- M.data_F %>% 
  mutate(Macaca_sur = ifelse(is.na(Macaca_sur) | Macaca_dist %in% "C", 0, Macaca_sur)) %>% 
  group_by(樣區編號, 獼猴樣區編號, Year, Month, Day, day_of_year) %>% 
  summarise(count = sum(as.numeric(Macaca_sur), na.rm = T), effort_n = length(樣點代號), .groups = "drop_last") %>% 
  bind_rows(  
    M.data %>% filter(!樣區編號 %in% paste0("A08-0", 1:9)) %>% 
      mutate(Macaca_sur = ifelse(is.na(Macaca_sur) | Macaca_dist %in% "C", 0, Macaca_sur)) %>% 
      anti_join(M.data_F[, c('獼猴樣區編號', "樣區編號", 'Year')], by = c('獼猴樣區編號', "樣區編號", 'Year')) %>% 
      group_by(樣區編號, 獼猴樣區編號, Year, Month, Day, day_of_year) %>% 
      summarise(count = sum(as.numeric(Macaca_sur)), effort_n = length(樣點代號), .groups = "drop_last")
  ) %>% 
  left_join(list_Site[, c('SiteID', '獼猴樣區編號', "樣區編號", "X_wgs84", "Y_wgs84")], by = c('獼猴樣區編號', "樣區編號")) %>% 
  filter(Year %in% c(2021:2025)) %>% 
  filter(!樣區編號 %in% c("A08-10")) %>% 
  st_as_sf(coords = c("X_wgs84", "Y_wgs84"), crs = 4326, remove = F) %>% 
  st_transform(crs = 3826) %>% 
  st_transform(crs = crs(r)) %>% 
  mutate(
    cell_id = cellFromXY(r, st_coordinates(geometry)),
    cell_id_5km = cellFromXY(r5km, st_coordinates(geometry))
  ) %>% 
  st_drop_geometry() %>% 
  left_join(environment_r_cell, by = c("cell_id" = "cell")) %>% 
  st_drop_geometry()

pred_grid <- environment_r_cell