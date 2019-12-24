#樣點資料檢核
#---- load library

library(data.table)
library(magrittr)
library(readxl)
library(tidyr)
library(purrr)
library(dplyr)
library(writexl)
library(rgdal)

#------------------------
col_names<- read_excel("./data/raw/BBSpointXY/all_20191224.xlsx",
                   sheet=1) %>% colnames

xy.19<- read_excel("./data/raw/BBSpointXY/all_20191224.xlsx",
           sheet=1) %>% setDT %>% 
  .[, list(樣區編號, 樣點代號, 縣市, X_經度, Y_緯度)] %>% 
  setnames(., c("Site_N", "Point", "County", "X", "Y")) %>% 
  .[, Site_N := as.character(Site_N)] %>% 
  .[, Point := as.numeric(Point)] %>% 
  .[, X :=as.numeric(X)] %>% 
  .[, Y :=as.numeric(Y)] %>% 
  .[, X := round(X, 5)] %>% 
  .[, Y := round(Y, 5)] 

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

xy.17<- read_excel("./data/raw/BBSpointXY/BBSpoint2017_管理者端.xlsx",
                   sheet="BBS樣點表(完整版)_管理者端",
                   col_names = F,
                   range="A5:O4780") %>% setDT %>% 
  setnames(., col_names) %>% 
  .[, list(樣區編號, 樣點代號, 縣市, X_經度, Y_緯度)] %>% 
  setnames(., c("Site_N", "Point", "County", "X", "Y"))%>% 
  .[, Site_N := as.character(Site_N)] %>% 
  .[, Point := as.numeric(Point)]  %>% 
  .[, X :=as.numeric(X)] %>% 
  .[, Y :=as.numeric(Y)] %>% 
  .[, X := round(X, 5)] %>% 
  .[, Y := round(Y, 5)] 

xy.14<- read_excel("./data/raw/BBSpointXY/2014樣點表.xls",
                   sheet= "BBS樣點表(完整版)",
                   col_names = F,
                   range="A4:O4337") %>% setDT %>% 
  setnames(., col_names) %>% 
  .[, list(樣區編號, 樣點代號, 縣市, X_經度, Y_緯度)] %>% 
  setnames(., c("Site_N", "Point", "County", "X", "Y"))%>% 
  .[, Site_N := as.character(Site_N)] %>% 
  .[, Point := as.numeric(Point)]  %>% 
  .[, X :=as.numeric(X)] %>% 
  .[, Y :=as.numeric(Y)] %>% 
  .[, X := round(X, 5)] %>% 
  .[, Y := round(Y, 5)] 
#------------------------
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
  
  
 S15 %>% setDT %>% .[, .N, by =  list(Year, Site_N, Point)] %>% View #check N<=2
 
 S15 %<>% 
   left_join(xy.19, by = c("Site_N", "Point"), suffix = c("", ".y")) %>% 
   left_join(xy.17, by = c("Site_N", "Point"), suffix = c("", ".y")) %>% 
   left_join(xy.14, by = c("Site_N", "Point"), suffix = c("", ".y")) %>%
   left_join(xy.del, by = c("Site_N", "Point"), suffix = c("", ".y")) %>%
   setDT %>% 
   .[is.na(X), X := X.y] %>%
   .[is.na(Y), Y := Y.y] %>%
   .[is.na(County), County := County.y] %>% 
   .[is.na(X), X := X.y.y] %>%
   .[is.na(Y), Y := Y.y.y] %>%
   .[is.na(County), County := County.y.y] %>% 
   .[is.na(X), X := X.y.y] %>%
   .[is.na(Y), Y := Y.y.y] %>%
   .[is.na(County), County := County.y.y] %>% 
   .[is.na(X), X := X.y.y.y] %>%
   .[is.na(Y), Y := Y.y.y.y] %>%
   .[is.na(County), County := County.y.y.y] %>% 
   .[, c("X.y", "Y.y", "County.y",
         "X.y.y", "Y.y.y", "County.y.y",
         "X.y.y.y", "Y.y.y.y", "County.y.y.y") := NULL] 

 write_xlsx(S15, "data/clean/Site/Site_2015_v2.xlsx")

 
 
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
 
 
 S16 %>% setDT %>% .[, .N, by =  list(Year, Site_N, Point)] %>% View #check N<=2

 
 S16.2<- 
   lapply(paste0("./data/raw/"), function(x){
     list.files(x, pattern = "BBSdata_2016", full.names = T) %>%  
       read_xlsx(., sheet = "birddata", cell_cols("D:AF")) %>% 
       setDT %>%
       .[時段 %in% c("A", "B"),] %>%
       #.[!(時段 %in% "Supplementary"),] %>%
       .[調查旅次編號 %in% c(1,2)] %>%
       #.[ 分析 %in% "Y",] %>%
       .[, list(年, 樣區編號, 樣點編號, 調查旅次編號, 月, 日, `開始時間（時）`, `開始時間（分）`)] %>%
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
 
 
 
 rbind(S16,S16.2) %>% setDT %>%.[, length(Survey), by =  c("Year", "Site_N", "Point")] %>% View
 
 S16 <- rbind(S16,S16.2)%>% 
   left_join(xy.19, by = c("Site_N", "Point"), suffix = c("", ".y")) %>% 
   left_join(xy.17, by = c("Site_N", "Point"), suffix = c("", ".y")) %>% 
   left_join(xy.14, by = c("Site_N", "Point"), suffix = c("", ".y")) %>%
   left_join(xy.del, by = c("Site_N", "Point"), suffix = c("", ".y")) %>%
   setDT  %>% 
   .[is.na(X), X := X.y] %>%
   .[is.na(Y), Y := Y.y] %>%
   .[is.na(County), County := County.y] %>% 
   .[is.na(X), X := X.y.y] %>%
   .[is.na(Y), Y := Y.y.y] %>%
   .[is.na(County), County := County.y.y] %>% 
   .[is.na(X), X := X.y.y] %>%
   .[is.na(Y), Y := Y.y.y] %>%
   .[is.na(County), County := County.y.y] %>% 
   .[is.na(X), X := X.y.y.y] %>%
   .[is.na(Y), Y := Y.y.y.y] %>%
   .[is.na(County), County := County.y.y.y]  %>% 
   .[, c("X.y", "Y.y", "County.y",
         "X.y.y", "Y.y.y", "County.y.y",
         "X.y.y.y", "Y.y.y.y", "County.y.y.y") := NULL]
 
 write_xlsx(S16, "data/clean/Site/Site_2016_v2.xlsx")
 
 
 
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
 

  S17 %>% setDT %>% .[, .N, by =  list(Year, Site_N, Point)] %>% View  #check N<=2 

  S17 %<>% 
    left_join(xy.19, by = c("Site_N", "Point"), suffix = c("", ".y")) %>% 
    left_join(xy.17, by = c("Site_N", "Point"), suffix = c("", ".y")) %>% 
    left_join(xy.14, by = c("Site_N", "Point"), suffix = c("", ".y")) %>%
    left_join(xy.del, by = c("Site_N", "Point"), suffix = c("", ".y")) %>%
    setDT  %>% 
    .[is.na(X), X := X.y] %>%
    .[is.na(Y), Y := Y.y] %>%
    .[is.na(County), County := County.y] %>% 
    .[is.na(X), X := X.y.y] %>%
    .[is.na(Y), Y := Y.y.y] %>%
    .[is.na(County), County := County.y.y] %>% 
    .[is.na(X), X := X.y.y] %>%
    .[is.na(Y), Y := Y.y.y] %>%
    .[is.na(County), County := County.y.y] %>% 
    .[is.na(X), X := X.y.y.y] %>%
    .[is.na(Y), Y := Y.y.y.y] %>%
    .[is.na(County), County := County.y.y.y] %>% 
    .[, c("X.y", "Y.y", "County.y",
          "X.y.y", "Y.y.y", "County.y.y",
          "X.y.y.y", "Y.y.y.y", "County.y.y.y") := NULL]
  
  write_xlsx(S17, "data/clean/Site/Site_2017_v2.xlsx")
 
  
#2018
  
  S18<- 
    lapply(paste0("./data/raw/"), function(x){
      list.files(x, pattern = "BBSdata_2018", full.names = T) %>%  
        read_xlsx(., sheet = "birddata", cell_cols("D:AF")) %>% 
        setDT %>%
        .[時段 %in% c("A", "B","a","b"),] %>%
        #.[!(時段 %in% "Supplementary"),] %>%
        .[調查旅次編號 %in% c(1,2)] %>%
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
  
  
  S18 %>% setDT %>% .[, .N, by =  list(Year, Site_N, Point)] %>% View  #check N<=2 

  
  S18 %<>% 
    left_join(xy.19, by = c("Site_N", "Point"), suffix = c("", ".y")) %>% 
    left_join(xy.17, by = c("Site_N", "Point"), suffix = c("", ".y")) %>% 
    left_join(xy.14, by = c("Site_N", "Point"), suffix = c("", ".y")) %>%
    left_join(xy.del, by = c("Site_N", "Point"), suffix = c("", ".y")) %>%
    setDT %>% 
    .[is.na(X), X := X.y] %>%
    .[is.na(Y), Y := Y.y] %>%
    .[is.na(County), County := County.y] %>% 
    .[is.na(X), X := X.y.y] %>%
    .[is.na(Y), Y := Y.y.y] %>%
    .[is.na(County), County := County.y.y] %>% 
    .[is.na(X), X := X.y.y] %>%
    .[is.na(Y), Y := Y.y.y] %>%
    .[is.na(County), County := County.y.y] %>% 
    .[is.na(X), X := X.y.y.y] %>%
    .[is.na(Y), Y := Y.y.y.y] %>%
    .[is.na(County), County := County.y.y.y]  %>% 
    .[, c("X.y", "Y.y", "County.y",
          "X.y.y", "Y.y.y", "County.y.y",
          "X.y.y.y", "Y.y.y.y", "County.y.y.y") := NULL] 
  
  write_xlsx(S18, "data/clean/Site/Site_2018_v2.xlsx")
  
  #2019
  S19 <- 
    read_xlsx("data/raw/2019獼猴調查(樣區整理)_分析用.xlsx",
              sheet = 2,
              col_types = "text") %>% 
    setDT %>% 
    .[, list(`樣點編號`)] %>% 
    separate("樣點編號", c("Site_a", "Site_b","Point"), "-") %>% setDT %>%
    .[, Site_N := paste(Site_a, Site_b, sep = "-")]  %>%
    .[, c("Site_a", "Site_b") := NULL] %>% 
    .[, Point := as.numeric(Point)]
  
  S19.survey.condition <- 
    read_xlsx("data/raw/2019獼猴調查(樣區整理)_分析用.xlsx",
              sheet = 1) %>% 
    setDT %>% 
    .[, list(`樣區\r\n編號`, 第一旅次, 第二旅次)] %>% 
    setnames(c("Site_N" ,"2019_1", "2019_2")) %>% 
    .[, list(`2019_1` = ifelse(is.na(`2019_1`), 0, 1),
             `2019_2` = ifelse(is.na(`2019_2`), 0, 1)),
      by = Site_N]
  
  S19 %<>% S19.survey.condition[., on = "Site_N"] %>% 
    melt(.,id.vars=c(1, 4),
         variable.name = "Survey",
         value.name = "Do.survey") %>% 
    separate("Survey", c("Year", "Survey"), "_") %>%
    setDT %>%
    .[ !(Do.survey %in% 0),] %>% 
    .[ ,list(Year, Site_N, Point, Survey)] %>%
    .[ ,c("Month", "Day", "Hour", "Minute") := NA]
  
  S19 %>% setDT %>% .[, .N, by =  list(Year, Site_N, Point)] %>% View  #check N<=2 
  
  S19 %<>% 
    left_join(xy.19, by = c("Site_N", "Point"), suffix = c("", ".y")) %>% 
    left_join(xy.17, by = c("Site_N", "Point"), suffix = c("", ".y")) %>% 
    left_join(xy.14, by = c("Site_N", "Point"), suffix = c("", ".y")) %>%
    left_join(xy.del, by = c("Site_N", "Point"), suffix = c("", ".y")) %>%
    setDT  %>% 
    .[is.na(X), X := X.y] %>%
    .[is.na(Y), Y := Y.y] %>%
    .[is.na(County), County := County.y] %>% 
    .[is.na(X), X := X.y.y] %>%
    .[is.na(Y), Y := Y.y.y] %>%
    .[is.na(County), County := County.y.y] %>% 
    .[is.na(X), X := X.y.y] %>%
    .[is.na(Y), Y := Y.y.y] %>%
    .[is.na(County), County := County.y.y] %>% 
    .[is.na(X), X := X.y.y.y] %>%
    .[is.na(Y), Y := Y.y.y.y] %>%
    .[is.na(County), County := County.y.y.y]  %>% 
    .[, c("X.y", "Y.y", "County.y",
          "X.y.y", "Y.y.y", "County.y.y",
          "X.y.y.y", "Y.y.y.y", "County.y.y.y") := NULL]
  
  write_xlsx(S19, "data/clean/Site/Site_2019_v1.xlsx")
  
  