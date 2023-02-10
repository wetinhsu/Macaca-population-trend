#樣點資料檢核
#---- load library

library(data.table)
library(tidyverse)
library(readxl)
library(writexl)
#------------------------
#最新版本的樣點表
xy.new<- read_excel("./data/raw/BBSpointXY/all_20220504.xlsx",
                    sheet="樣點表") %>% 
  select(樣區編號, 樣點代號, 縣市, X_經度, Y_緯度) %>% 
  setNames(., c("Site_N", "Point", "County", "X", "Y")) %>% 
  filter(!is.na(Site_N)) %>% 
  mutate(Site_N = as.character(Site_N)) %>% 
  mutate(Point = as.numeric(Point)) %>% 
  mutate_at(c("X", "Y"), function(x) {x %>% as.numeric %>% round(5)})
  



#前一版本的樣點表
xy.last<- read_excel("./data/raw/BBSpointXY/all_20191224.xlsx",
                     sheet=1) %>% setDT %>% 
  .[, list(樣區編號, 樣點代號, 縣市, X_經度, Y_緯度)] %>% 
  setnames(., c("Site_N", "Point", "County", "X", "Y")) %>% 
  .[, Site_N := as.character(Site_N)] %>% 
  .[, Point := as.numeric(Point)] %>% 
  .[, X :=as.numeric(X)] %>% 
  .[, Y :=as.numeric(Y)] %>% 
  .[, X := round(X, 5)] %>% 
  .[, Y := round(Y, 5)] 



#被刪除的樣點
xy.del<- read_excel("./data/raw/BBSpointXY/已被刪除的樣點表.xlsx",
                    sheet=1) %>% setDT %>% 
  .[, list(樣區編號, 樣點代號, 縣市, X_經度, Y_緯度)] %>% 
  setnames(., c("Site_N", "Point", "County", "X", "Y")) %>% 
  .[, Site_N := as.character(Site_N)] %>% 
  .[, Point := as.numeric(Point)] %>% 
  .[, X :=as.numeric(X)] %>% 
  .[, Y :=as.numeric(Y)] %>% 
  .[, X := round(X, 5)] %>% 
  .[, Y := round(Y, 5)] 


xy.del<- #update後的刪除後樣點
  xy.new %>% 
  full_join(xy.last, by = c("Site_N", "Point"), suffix = c("", ".y")) %>% 
  filter(is.na(X)|is.na(Y)) %>%  #把last有的，但是new沒有的資料挑出並放進xy.del
  select(-X, -Y, -County) %>% 
  setnames(., c("Site_N", "Point", "County", "X", "Y")) %>% 
  bind_rows(xy.del ,.)


#--------

#dfo小柯給的資料

dfo  <- data.table::fread("D:/R/test/bbs_handover_temp_v20190123 - WT/dfs2.csv",
              head=T, encoding = "UTF-8", na.strings = "")   #UTF-8

S1521 <- 
dfo %>% 
  filter(年 %in% 2015:2021) %>% 
  filter(! 時段  %in% c("Supplementary")) %>% 
  select(年, 樣區編號, 樣點編號, 調查旅次編號, 月, 日, `開始時間（時）`, `開始時間（分）`) %>% 
  setnames(., c("Year", "Site_N", "Point",  "Survey", "Month", "Day", "Hour", "Minute")) %>% 
  mutate(Point = as.numeric(Point)) %>% 
  unique() %>% 
  mutate(Point = case_when(
    Year %in% 2018 &Site_N %in% "B10-03" & Point  %in% c(1:8) ~ as.numeric(Point) + 10,
    Year %in% 2018 &Site_N %in% "B10-13" & Point  %in% c(1:8) ~ as.numeric(Point) + 10,
    Year %in% 2018 &Site_N %in% "B10-14" & Point  %in% c(1:12) ~ as.numeric(Point) + 10,
    TRUE ~ Point
  ))  %>% 
left_join(xy.new, by = c("Site_N",
                          "Point"), suffix = c("", ".y")) %>% 
  left_join(xy.del, by = c("Site_N", "Point"), suffix = c("", ".y")) %>%
  arrange(`X`,`X.y`) %>%  
  mutate(X = ifelse(is.na(X),  X.y, X)) %>% 
  mutate(Y = ifelse(is.na(Y),  Y.y, Y)) %>%
  mutate(County = ifelse(is.na(County),  County.y, County)) %>%
  select(-ends_with(".y"))  

write_xlsx(S1521, "研討會_202307/data/clean/Site_1521_v1.xlsx")
