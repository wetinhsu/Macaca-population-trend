#樣點資料檢核
#---- load library

library(data.table)
library(tidyverse)
library(readxl)
library(writexl)
#------------------------
#最新版本的樣點表
xy.new<- read_excel("./data/raw/BBSpointXY/all_20210312.xlsx",
                    sheet="樣點表") %>% setDT %>% 
  .[, list(樣區編號, 樣點代號, 縣市, X_經度, Y_緯度)] %>% 
  setnames(., c("Site_N", "Point", "County", "X", "Y")) %>% 
  .[, Site_N := as.character(Site_N)] %>% 
  .[!is.na(Site_N),] %>% 
  .[, Point := as.numeric(Point)] %>% 
  .[, X :=as.numeric(X)] %>% 
  .[, Y :=as.numeric(Y)] %>% 
  .[, X := round(X, 5)] %>% 
  .[, Y := round(Y, 5)] 



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

#2020
S20 <- 
  lapply(paste0("./data/raw/"), function(x){
    list.files(x, pattern = "BBSdata_2020", full.names = T) %>%  
      read_xlsx(., sheet = "birddata", cell_cols("D:AF"), col_types ="text") %>% 
      setDT %>%
      #.[時段 %in% c("0-3minutes", "3-6minutes"),] %>%
      .[!(時段 %in% "Supplementary"),] %>%
      .[調查旅次編號 %in% c("1","2")] %>%
      #.[ 分析 %in% "Y",] %>% 
      .[,list(年, 樣區編號, 樣點編號, 調查旅次編號, 月, 日, `開始時間（時）`, `開始時間（分）`)] %>%
      setnames(., c("Year", "Site_N", "Point",  "Survey", "Month", "Day", "Hour", "Minute")) %>%
      .[!duplicated(.)]  
  } ) %>% 
  do.call(rbind, .) %>% 
  data.table%>%
  .[, Site_N := as.character(Site_N)] %>%
  .[, Point := as.numeric(Point)] %>%
  
  #debug~
  
  
  
  #~debug
  
  .[, DATE := as.IDate(paste(Year, Month, Day, sep = "/"))] %>% 
  .[, DD := as.ITime(paste(Hour, Minute,sep = ":"))] %>% 
  .[, Time:= as.ITime(min(DD)), by = list(Year, Site_N, Point, DATE)] %>%
  .[, Survey := as.numeric(Survey)] %>%
  .[, Month := as.numeric(Month)] %>%
  .[, Day := as.numeric(Day)] %>%
  .[, Hour := as.numeric(hour(Time))] %>%
  .[, Minute := as.numeric(minute(Time))] %>%
  .[, c("DD", "DATE", "Time") := NULL] %>%
  .[!duplicated(.)]
S20 %>% setDT %>% .[, .N, by =  list(Year, Site_N, Point)] %>% .[N>2,]  #check N<=2 

S20 %<>% 
  left_join(xy.new, by = c("Site_N", "Point"), suffix = c("", ".y")) %>% 
  left_join(xy.del, by = c("Site_N", "Point"), suffix = c("", ".y")) %>%
  setDT  %>%  
  .[is.na(X), X := X.y] %>%
  .[is.na(Y), Y := Y.y] %>%
  .[is.na(County), County := County.y] %>%
  
  .[, c("X.y", "Y.y", "County.y") := NULL]

write_xlsx(S20, "data/clean/Site/Site_2020_v1.xlsx")


#------------------------
#2019
S19 <- 
  lapply(paste0("./data/raw/BBSdata/", 2019), function(x){
    list.files(x, pattern = "BBSdata_", full.names = T) %>%  
      read_xlsx(., sheet = "birddata", cell_cols("A:AF"), col_types ="text") %>% 
      setDT %>%
      #.[時段 %in% c("0-3minutes", "3-6minutes"),] %>%
      .[!(時段 %in% "Supplementary"),] %>%
      .[調查旅次編號 %in% c("1","2")] %>%
      .[! 分析 %in% "N",] %>% 
      .[,list(年, 樣區編號, 樣點編號, 調查旅次編號, 月, 日, `開始時間（時）`, `開始時間（分）`)] %>%
      setnames(., c("Year", "Site_N", "Point",  "Survey", "Month", "Day", "Hour", "Minute")) %>%
      .[!duplicated(.)]  
  } ) %>% 
  do.call(rbind, .) %>% 
  data.table%>%
  .[, Site_N := as.character(Site_N)] %>%
  .[, Point := as.numeric(Point)] %>%
  
  #debug~
  
  
  
  #~debug
  
  .[, DATE := as.IDate(paste(Year, Month, Day, sep = "/"))] %>% 
  .[, DD := as.ITime(paste(Hour, Minute,sep = ":"))] %>% 
  .[, Time:= as.ITime(min(DD)), by = list(Year, Site_N, Point, DATE)] %>%
  .[, Survey := as.numeric(Survey)] %>%
  .[, Month := as.numeric(Month)] %>%
  .[, Day := as.numeric(Day)] %>%
  .[, Hour := as.numeric(hour(Time))] %>%
  .[, Minute := as.numeric(minute(Time))] %>%
  .[, c("DD", "DATE", "Time") := NULL] %>%
  .[!duplicated(.)]
S19 %>% setDT %>% .[, .N, by =  list(Year, Site_N, Point)] %>% .[N>2,]  #check N<=2 

S19 %<>% 
  left_join(xy.new, by = c("Site_N", "Point"), suffix = c("", ".y")) %>% 
  left_join(xy.del, by = c("Site_N", "Point"), suffix = c("", ".y")) %>% 
  setDT  %>%  
  .[is.na(X), X := X.y] %>%
  .[is.na(Y), Y := Y.y] %>%
  .[is.na(County), County := County.y] %>%
  
  .[, c("X.y", "Y.y", "County.y") := NULL]

write_xlsx(S19, "data/clean/Site/Site_2019_v3.xlsx")



#2018

S18<- 
  lapply(paste0("./data/raw/BBSdata/", 2018), function(x){
    list.files(x, pattern = "BBSdata_", full.names = T) %>%  
      read_xlsx(., sheet = "birddata", cell_cols("D:AF"), col_types ="text") %>% 
      setDT %>%
      .[!(時段 %in% "Supplementary"),] %>%
      .[調查旅次編號 %in% c("1","2")] %>%
      #.[ 分析 %in% "Y",] %>%
      .[, list(年, 樣區編號, 樣點編號, 調查旅次編號, 月, 日, `開始時間（時）`, `開始時間（分）`)] %>%
      setnames(., c("Year", "Site_N", "Point",  "Survey", "Month", "Day", "Hour", "Minute")) %>%
      .[!duplicated(.)]  
  } ) %>% 
  do.call(rbind, .) %>% 
  data.table%>%
  .[, Site_N := as.character(Site_N)] %>%
  .[, Point := as.numeric(Point)] %>%
  
  #debug~
  .[ Site_N %in% "B10-03" & Point  %in% c(1:8),  Point := (Point + 10)] %>%  
  .[ Site_N %in% "B10-13" & Point  %in% c(1:8),  Point := (Point + 10)] %>% 
  .[ Site_N %in% "B10-14" & Point  %in% c(1:12),  Point := (Point + 10)] %>% 
  #~debug
  
  .[, DATE := as.IDate(paste(Year, Month, Day, sep = "/"))] %>% 
  .[, DD := as.ITime(paste(Hour, Minute,sep = ":"))] %>% 
  .[, Time:= as.ITime(min(DD)), by = list(Year, Site_N, Point, DATE)] %>%
  .[, Survey := as.numeric(Survey)] %>%
  .[, Month := as.numeric(Month)] %>%
  .[, Day := as.numeric(Day)] %>%
  .[, Hour := as.numeric(hour(Time))] %>%
  .[, Minute := as.numeric(minute(Time))] %>%
  .[, c("DD", "DATE", "Time") := NULL] %>%
  .[!duplicated(.)]


S18 %>% setDT %>% .[, .N, by =  list(Year, Site_N, Point)] %>% .[N>2,]  #check N<=2 


S18 %<>% 
  left_join(xy.new, by = c("Site_N", "Point"), suffix = c("", ".y")) %>% 
  left_join(xy.del, by = c("Site_N", "Point"), suffix = c("", ".y")) %>% 
  setDT  %>%  
  .[is.na(X), X := X.y] %>%
  .[is.na(Y), Y := Y.y] %>%
  .[is.na(County), County := County.y] %>%
  
  .[, c("X.y", "Y.y", "County.y") := NULL]

write_xlsx(S18, "data/clean/Site/Site_2018_v4.xlsx")



#2017 

S17 <- 
  lapply(paste0("./data/raw/BBSdata/", 2017), function(x){
    list.files(x, pattern = "BBSdata_", full.names = T) %>%  
      read_xlsx(., sheet = "birddata", cell_cols("D:AF")) %>% 
      setDT %>%
      #.[時段 %in% c("0-3minutes", "3-6minutes"),] %>%
      .[!(時段 %in% "Supplementary"),] %>%
      .[調查旅次編號 %in% c(1,2)] %>%
      .[ 分析 %in% "Y", list(年, 樣區編號, 樣點編號, 調查旅次編號, 月, 日, `開始時間（時）`, `開始時間（分）`)] %>%
      setnames(., c("Year", "Site_N", "Point",  "Survey", "Month", "Day", "Hour", "Minute")) %>%
      .[!duplicated(.)]  
  } ) %>% 
  do.call(rbind, .) %>% 
  data.table%>%
  .[, Site_N := as.character(Site_N)] %>%
  .[, Point := as.numeric(Point)] %>%
  
  #debug~
  .[  Year %in% 2017 & Site_N %in% "A20-02" & Point %in% 6 & Survey %in% 1, Point := 7] %>%
  .[  Year %in% 2017 & Site_N %in% "A20-02" & Point %in% 7 & Survey %in% 1, Point := 9] %>%
  .[  Year %in% 2017 & Site_N %in% "A20-02" & Point %in% 8 & Survey %in% 1, Point := 10] %>%
  
  #~debug
  
  .[, DATE := as.IDate(paste(Year, Month, Day, sep = "/"))] %>% 
  .[, DD := as.ITime(paste(Hour, Minute,sep = ":"))] %>% 
  .[, Time:= as.ITime(min(DD)), by = list(Year, Site_N, Point, DATE)] %>%
  .[, Survey := as.numeric(Survey)] %>%
  .[, Month := as.numeric(Month)] %>%
  .[, Day := as.numeric(Day)] %>%
  .[, Hour := as.numeric(hour(Time))] %>%
  .[, Minute := as.numeric(minute(Time))] %>%
  .[, c("DD", "DATE", "Time") := NULL] %>%
  .[!duplicated(.)]


S17 %>% setDT %>% .[, .N, by =  list(Year, Site_N, Point)] %>% .[N>2,]  #check N<=2 

S17 %<>% 
  left_join(xy.new, by = c("Site_N", "Point"), suffix = c("", ".y")) %>% 
  left_join(xy.del, by = c("Site_N", "Point"), suffix = c("", ".y")) %>% 
  setDT  %>%  
  .[is.na(X), X := X.y] %>%
  .[is.na(Y), Y := Y.y] %>%
  .[is.na(County), County := County.y] %>% 
  
  .[, c("X.y", "Y.y", "County.y") := NULL]

write_xlsx(S17, "data/clean/Site/Site_2017_v2.xlsx")


#2016 

S16 <- 
  lapply(paste0("./data/raw/BBSdata/", 2016), function(x){
    list.files(x, pattern = "BBSdata_", full.names = T) %>%  
      read_xlsx(., sheet = "birddata", cell_cols("D:AF")) %>% 
      setDT %>%
      #.[時段 %in% c("0-3minutes", "3-6minutes"),] %>%
      .[!(時段 %in% "Supplementary"),] %>%
      .[調查旅次編號 %in% c(1,2)] %>%
      .[ 分析 %in% "Y", list(年, 樣區編號, 樣點編號, 調查旅次編號, 月, 日, `開始時間（時）`, `開始時間（分）`)] %>%
      setnames(., c("Year", "Site_N", "Point",  "Survey", "Month", "Day", "Hour", "Minute")) %>%
      .[!duplicated(.)]  
  } ) %>% 
  do.call(rbind, .) %>% 
  data.table%>%
  .[, Site_N := as.character(Site_N)] %>%
  .[, Point := as.numeric(Point)] %>%
  
  #debug~
  .[  Year %in% 2016 & Site_N %in% "A09-20" & Month %in% 5, Survey:= 2] %>%
  .[  Year %in% 2016 & Site_N %in% "A12-01", Day := 9] %>%
  .[  Year %in% 2016 & Site_N %in% "A04-37", Day := 14] %>%
  .[!(Year %in% 2016 & Site_N %in% "A37-03" & Day %in% c(16,29)), ] %>%  
  .[  Year %in% 2016 & Site_N %in% "A34-47" & Day %in% 1, Day := 31] %>%
  .[  Year %in% 2016 & Site_N %in% "A04-53" & Day %in% 30, Day := 28] %>%
  .[  Year %in% 2016 & Site_N %in% "A27-24" & Day %in% 14, Day := 13] %>%
  .[  Year %in% 2016 & Site_N %in% "A27-24" & Day %in% 14 & Minute %in% 4, Minute := 9] %>% 
  #~debug
  
  .[, DATE := as.IDate(paste(Year, Month, Day, sep = "/"))] %>% 
  .[, DD := as.ITime(paste(Hour, Minute,sep = ":"))] %>% 
  .[, Time:= as.ITime(min(DD)), by = list(Year, Site_N, Point, DATE)] %>%
  .[, Survey := as.numeric(Survey)] %>%
  .[, Month := as.numeric(Month)] %>%
  .[, Day := as.numeric(Day)] %>%
  .[, Hour := as.numeric(hour(Time))] %>%
  .[, Minute := as.numeric(minute(Time))] %>%
  .[, c("DD", "DATE", "Time") := NULL]%>%
  .[!duplicated(.)] 


S16 %>% setDT %>% .[, .N, by =  list(Year, Site_N, Point)] %>% .[N>2,] #check N<=2


S16%<>% 
  left_join(xy.new, by = c("Site_N", "Point"), suffix = c("", ".y")) %>% 
  left_join(xy.del, by = c("Site_N", "Point"), suffix = c("", ".y")) %>% 
  setDT  %>%  
  .[is.na(X), X := X.y] %>%
  .[is.na(Y), Y := Y.y] %>%
  .[is.na(County), County := County.y] %>% 
  
  .[, c("X.y", "Y.y", "County.y") := NULL]

write_xlsx(S16, "data/clean/Site/Site_2016_v2.xlsx")

#2015

S15 <- 
  lapply(paste0("./data/raw/BBSdata/", 2015), function(x){
    list.files(x, pattern = "BBSdata_", full.names = T) %>%  
      read_xlsx(., sheet = "birddata", cell_cols("D:AF")) %>% 
      setDT %>%
      #.[時段 %in% c("0-3minutes", "3-6minutes"),] %>%
      .[!(時段 %in% "Supplementary"),] %>%
      .[調查旅次編號 %in% c(1,2)] %>%
      .[ 分析 %in% "Y", list(年, 樣區編號, 樣點編號, 調查旅次編號, 月, 日, `開始時間（時）`, `開始時間（分）`)] %>%
      setnames(., c("Year", "Site_N", "Point",  "Survey", "Month", "Day", "Hour", "Minute")) %>%
      .[!duplicated(.)]  
  } ) %>% 
  do.call(rbind, .) %>% 
  data.table%>%
  .[, Site_N := as.character(Site_N)] %>%
  .[, Point := as.numeric(Point)] %>%
  
  .[, DATE := as.IDate(paste(Year, Month, Day, sep = "/"))] %>% 
  .[, DD := as.ITime(paste(Hour, Minute,sep = ":"))] %>% 
  .[, Time:= as.ITime(min(DD)), by = list(Year, Site_N, Point, DATE)] %>%
  .[, Survey := as.numeric(Survey)] %>%
  .[, Month := as.numeric(Month)] %>%
  .[, Day := as.numeric(Day)] %>%
  .[, Hour := as.numeric(hour(Time))] %>%
  .[, Minute := as.numeric(minute(Time))] %>%
  .[, c("DD", "DATE", "Time") := NULL] %>%
  .[!duplicated(.)]


S15 %>% setDT %>% .[, .N, by =  list(Year, Site_N, Point)] %>% .[N>2,] #check N<=2

S15 %<>% 
  left_join(xy.new, by = c("Site_N", "Point"), suffix = c("", ".y")) %>% 
  left_join(xy.del, by = c("Site_N", "Point"), suffix = c("", ".y")) %>% 
  setDT  %>%  
  .[is.na(X), X := X.y] %>%
  .[is.na(Y), Y := Y.y] %>%
  .[is.na(County), County := County.y] %>%
  
  .[, c("X.y", "Y.y", "County.y") := NULL]

write_xlsx(S15, "data/clean/Site/Site_2015_v2.xlsx")


