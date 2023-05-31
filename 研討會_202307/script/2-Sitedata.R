
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
  select(ID, 樣區編號, 樣點代號, X_97, Y_97) %>% 
  setNames(., str_replace_all(colnames(.), c("樣區編號" = "Site_N",
                                             "樣點代號" = "Point"))) %>% 
  mutate_at(c("Point", "X_97", "Y_97"), as.integer)


#--------------------------------------------------------------------
S22 <- 
  read_xlsx("./data/raw/BBSdata_2022資料統整_20230516.xlsx",
            sheet = "birddata", col_types ="text") %>% 
  select(-ends_with("ori"))%>% 
  filter(! 時段  %in% c("Supplementary")) %>% 
  select(年, 樣區編號, 樣點編號, 調查旅次編號,調查者, 月, 日, `開始時間（時）`, `開始時間（分）`) %>% 
  setNames(., c("Year", "Site_N", "Point",  "Survey", "Surveyor","Month", "Day", "Hour", "Minute")) %>% 
  filter(! str_detect(Point, "X")) %>% 
  mutate(Point = as.numeric(Point)) %>% 
  mutate_at(c("Month", "Day", "Hour", "Minute"),function(x){ 
    ifelse(str_detect(x, "\\d"), x, NA )}) %>% 
  unique() %>% 
  group_by(Year, Survey, Site_N, Point) %>% 
  arrange(Hour, Minute, .by_group = T) %>% 
  slice(1) %>% 
  ungroup() %>% 
  
  left_join(xy.now, by = c("Site_N",
                           "Point"), suffix = c("", ".y")) %>% 
  select(-ends_with(".y"))  %>% 
  
  filter(Survey %in% c(1:2)) %>% 
  filter( !Site_N %in% c("A01-11","A01-14","A01-15","A01-16","A01-17",
                         "A04-62","A04-63","A04-64","A04-65","A04-66",
                         "A04-67","A04-68")) #陽管處計畫2022年時沒有執行獼猴調查


write_xlsx(S22, "data/clean/Site/Site_2022_v1.xlsx")

#--------------------------------------------------------------------
#dfo小柯給的資料

dfo  <- data.table::fread("D:/R/test/bbs_handover_temp_v20190123 - WT/dfs2.csv",
                          head=T, encoding = "UTF-8", na.strings = "")   #UTF-8

S1521 <- 
  dfo %>% 
  filter(年 %in% 2015:2021) %>% 
  filter(! 時段  %in% c("Supplementary")) %>% 
  select(年, 樣區編號, 樣點編號, 調查旅次編號,調查者, 月, 日, `開始時間（時）`, `開始時間（分）`) %>% 
  setNames(., c("Year", "Site_N", "Point",  "Survey", "Surveyor", "Month", "Day", "Hour", "Minute")) %>% 
  mutate(Point = as.numeric(Point)) %>% 
  unique() %>% 
  mutate(Point = case_when(
    Year %in% 2018 &Site_N %in% "B10-03" & Point  %in% c(1:8) ~ as.numeric(Point) + 10,
    Year %in% 2018 &Site_N %in% "B10-13" & Point  %in% c(1:8) ~ as.numeric(Point) + 10,
    TRUE ~ Point
  ))  %>% 
  group_by(Year, Survey, Site_N, Point) %>% 
  arrange(Hour, Minute, .by_group = T) %>% 
  slice(1) %>% 
  ungroup() %>% 
  
  
  left_join(xy.now, by = c("Site_N",
                           "Point"), suffix = c("", ".y")) %>% 
  select(-ends_with(".y"))  

S1522 <- 
S22 %>%
  mutate_if(is.numeric, as.character) %>% 
  bind_rows(S1521 %>% mutate_if(is.numeric, as.character)  )
  


  write_xlsx(S1522, "研討會_202307/data/clean/Site_1522_v1.xlsx")
  