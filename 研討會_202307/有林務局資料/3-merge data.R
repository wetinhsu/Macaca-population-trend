#---- load library
library(readxl)
library(writexl)
library(tidyverse)
library(sf)

#-----------------------------
#read point data

S.all<- "./研討會_202307/data/clean/Site_1522_v1.xlsx" %>%
  read_xlsx(., sheet = 1, col_types = "text")



#--------------------------------


M.all <- read_excel("./研討會_202307/data/clean/Macaca_1522_v1.xlsx", col_types = "text")

#--------------------------------------------------
library(DBI)

con <- dbConnect(RSQLite::SQLite(),  "D:/R/test/DB/P_BBS.db")
list_Point<-
  dbReadTable(con, "list_Point") %>% 
  arrange(樣區編號, 獼猴樣區編號,as.numeric(樣點代號)) 


dbDisconnect(con)




F.all <- 
  read_excel("./研討會_202307/有林務局資料/data/OnlyForestydata_20-22.xlsx",
             col_types = "text") %>% 
  left_join(.,list_Point, by = c( 'Site_N'= '獼猴樣區編號', 'Point' = '樣點代號')) %>% 
  select(Year:Minute,Site_N = 樣區編號.x, Macaca_sur = Macaca_sur.ori, Macaca_dist,ID ,X_97, Y_97) %>% 
              mutate( ID = ID %>% as.character)




#------------------------------------------------
M.data <- M.all %>% 
  full_join(S.all, by = c("Year", "Site_N", "Point", "Survey")) %>% 
  mutate(DataSource = "BBS") %>% 
  bind_rows(
  anti_join(F.all, ., by = c("Year", "Site_N", "Survey")) %>% 
    mutate(DataSource = "Foresty")
  ) %>% 
  
  filter(! str_detect(Site_N, "KIN")) %>% #exculde kiman
  filter(!(Site_N %in%  paste0("A08-0", 1:9) )) %>% #exclude蘭嶼
  filter(! is.na(X_97)) %>%
  arrange(Year,Site_N)

M.data %>%  #確認每一旅次內的每一個樣點資料只有1筆
  group_by(Year, Survey, Site_N, Point) %>% 
  summarise( n=n()) %>% 
  filter(n >1) %>% View

#--------------------------------



write_xlsx(M.data, "./研討會_202307/有林務局資料/data/clean/merge_data_1522.xlsx")
