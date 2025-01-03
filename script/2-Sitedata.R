
library(tidyverse)
library(sf)
library(DBI)
library(dbx)
library(readxl)
library(writexl)
#--------------------------------------------------------------------
con <- dbxConnect(adapter="sqlite", dbname="D:/R/test/DB/P_BBS.db")
list_Point<-
  dbReadTable(con, "list_Point") %>% 
  arrange(樣區編號, 獼猴樣區編號,as.numeric(樣點代號)) 


dbDisconnect(con)

#--------------------------------------------------------------------
xy.now <- 
  list_Point %>% 
  select(PointID, 樣區編號, 樣點代號, X_97, Y_97) %>% 
  setNames(., str_replace_all(colnames(.), c("樣區編號" = "Site_N",
                                             "樣點代號" = "Point"))) %>% 
  mutate_at(c("Point", "X_97", "Y_97"), as.integer)


#--------------------------------------------------------------------
# S22 <- 
#   read_xlsx("./data/raw/BBSdata_2022資料統整_20230516.xlsx",
#             sheet = "birddata", col_types ="text") %>% 
#   select(-ends_with("ori"))%>% 
#   filter(! 時段  %in% c("Supplementary")) %>% 
#   select(年, 樣區編號, 樣點編號, 調查旅次編號, 月, 日, `開始時間（時）`, `開始時間（分）`) %>% 
#   setNames(., c("Year", "Site_N", "Point",  "Survey", "Month", "Day", "Hour", "Minute")) %>% 
#   filter(! str_detect(Point, "X")) %>% 
#   mutate(Point = as.numeric(Point)) %>% 
#   unique() %>% 
#   mutate(YSSP = paste(Year, Survey, Site_N, Point, sep = "_")) %>% 
#   split(., .$YSSP) %>% 
#   map(., function(x){
#     x %>% 
#       arrange(Hour, Minute) %>% 
#       slice(1)
#   }) %>% 
#   bind_rows() %>% 
#   select(-YSSP)%>% 
#   
#   
#   left_join(xy.now, by = c("Site_N",
#                            "Point"), suffix = c("", ".y")) %>% 
#   select(-ends_with(".y"))  
# 
# 
# write_xlsx(S22, "data/clean/Site/Site_2022_v1.xlsx")

#--------------------------------------------------------------------
#dfo小柯給的資料

dfo  <- data.table::fread("D:/R/test/bbs_handover_temp_v20190123 - WT/dfs2.csv",
                          head=T, encoding = "UTF-8", na.strings = "")   #UTF-8

S1523<- 
  dfo %>% 
  filter(年 %in% 2015:2023) %>% 
  filter(! 時段  %in% c("Supplementary")) %>% 
  select(年, 樣區編號, 樣點編號, 調查旅次編號, 月, 日, `開始時間（時）`, `開始時間（分）`) %>% 
  setNames(., c("Year", "Site_N", "Point",  "Survey", "Month", "Day", "Hour", "Minute")) %>% 
  mutate(Point = as.numeric(Point)) %>% 
  unique() %>% 
  mutate(Point = case_when(
    Year %in% 2018 &Site_N %in% "B10-03" & Point  %in% c(1:8) ~ as.numeric(Point) + 10,
    Year %in% 2018 &Site_N %in% "B10-13" & Point  %in% c(1:8) ~ as.numeric(Point) + 10,
    TRUE ~ Point
  ))  %>%
  mutate(YSSP = paste(Year, Survey, Site_N, Point, sep = "_")) %>% 
  split(., .$YSSP) %>% 
  map(., function(x){
    x %>% 
      arrange(Hour, Minute) %>% 
      slice(1)
  }) %>% 
  bind_rows() %>% 
  select(-YSSP)%>% 
  
  
  left_join(xy.now, by = c("Site_N",
                           "Point"), suffix = c("", ".y")) %>% 
  select(-ends_with(".y"))  %>% 
  filter( !(Year %in% c(2022,2023) & 
              Site_N %in% c("A01-11","A01-14","A01-15","A01-16","A01-17",
                         "A04-62","A04-63","A04-64","A04-65","A04-66",
                         "A04-67","A04-68")
            )
          ) #陽管處計畫2022、2023年時沒有執行獼猴調查，只做鳥類調查。


