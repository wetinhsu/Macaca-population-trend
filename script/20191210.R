
#---- load library

library(data.table)
library(magrittr)
library(readxl)
library(tidyr)
library(purrr)
library(dplyr)
library(writexl)
library(rgdal)


#step1==================================================================================


S1517 <- 
  lapply(paste0("./data/raw/BBSdata/", 2015:2017), function(x){
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
  .[, Point := as.numeric(Point)]%>%

  #debug~
  .[  Year %in% 2016 & Site_N %in% "A09-20" & Month %in% 5, Survey:= 2] %>%
  .[  Year %in% 2016 & Site_N %in% "A12-01", Day := 9] %>%
  .[  Year %in% 2016 & Site_N %in% "A04-37", Day := 14] %>%
  .[!(Year %in% 2016 & Site_N %in% "A37-03" & Day %in% c(16,29)), ] %>%  
  .[  Year %in% 2016 & Site_N %in% "A34-47" & Day %in% 1, Day := 31] %>%
  .[  Year %in% 2016 & Site_N %in% "A04-53" & Day %in% 30, Day := 28] %>%
  .[  Year %in% 2016 & Site_N %in% "A27-24" & Day %in% 14, Day := 13] %>%
  .[  Year %in% 2017 & Site_N %in% "A20-02" & Point %in% 6 & Survey %in% 1, Point := 7] %>%
  .[  Year %in% 2017 & Site_N %in% "A20-02" & Point %in% 7 & Survey %in% 1, Point := 9] %>%
  .[  Year %in% 2017 & Site_N %in% "A20-02" & Point %in% 8 & Survey %in% 1, Point := 10] %>%
  #~debug 
  
  .[, DATE := as.IDate(paste(Year, Month, Day, sep = "/"))] %>% 
  .[, DD := as.ITime(paste(Hour, Minute,sep = ":"))] %>% 
  .[, Time:= as.ITime(min(DD)), by = list(Year, Site_N, Point, DATE)] %>%
  .[, c("DD", "Hour", "Minute") := NULL] %>%
  .[!duplicated(.)]

S1517 %>% setDT %>%.[, length(Survey), by =  c("Year", "Site_N", "Point")] %>% View

#===========================================================
#臨時的data

S16.2 <- 
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
  .[ Site_N %in% "A12-01", Day := 9] %>%
  .[ Site_N %in% "A04-37", Day := 14] %>%
  .[!(Site_N %in% "A37-03" & Day %in% c(16,29)), ] %>%  
  .[ Site_N %in% "A34-47" & Day %in% 1, Day := 31] %>%
  .[ Site_N %in% "A04-53"& Day %in% 30, Day := 28] %>%
  .[ Site_N %in% "A27-24"& Day %in% 14, Day := 13] %>%
  .[, DATE := as.IDate(paste(Year, Month, Day, sep = "/"))] %>% 
  .[, DD := as.ITime(paste(Hour, Minute,sep = ":"))] %>% 
  .[, Time:= as.ITime(min(DD)), by = list(Year, Site_N, Point, DATE)] %>%
  
  .[, c("DD", "Hour", "Minute") := NULL] %>%
  .[!duplicated(.)] 

S16.2 %>% setDT %>%.[, length(Survey), by =  c("Year", "Site_N", "Point")] %>% View

#======================================================================================
#臨時的data

S18 <- 
  lapply(paste0("./data/raw/"), function(x){
    list.files(x, pattern = "BBSdata_2018", full.names = T) %>%  
      read_xlsx(., sheet = "birddata", cell_cols("D:AF")) %>% 
      setDT %>%
      .[時段 %in% c("A", "B"),] %>%
      #.[!(時段 %in% "Supplementary"),] %>%
      .[調查旅次編號 %in% c(1,2)] %>%
      #.[ 分析 %in% "Y",] %>%
      .[ , list(年, 樣區編號, 樣點編號, 調查旅次編號, 月, 日, `開始時間（時）`, `開始時間（分）`)] %>%
      setnames(., c("Year", "Site_N", "Point",  "Survey", "Month", "Day", "Hour", "Minute")) %>%
      .[!duplicated(.)]  
  } ) %>% 
  do.call(rbind, .) %>% 
  data.table%>%
  .[, Site_N := as.character(Site_N)] %>%
  .[, Point := as.numeric(Point)]%>%
  .[, DATE := as.IDate(paste(Year, Month, Day, sep = "/"))] %>% 
  .[, DD := as.ITime(paste(Hour, Minute,sep = ":"))] %>% 
  .[, Time:= as.ITime(min(DD)), by = list(Year, Site_N, Point, DATE)] %>%
  .[, c("DD", "Hour", "Minute") := NULL] %>%
  .[!duplicated(.)] 


S18 %>% setDT %>%.[, length(Survey), by =  c("Year", "Site_N", "Point")] %>% View

S18.survey.condition <- 
  read_xlsx("data/raw/2018獼猴調查(樣區整理)_分析用.xlsx",
            sheet = 1) %>% 
  setDT %>% 
  .[, list(`樣區\r\n編號`, 第一旅次, 第二旅次)] %>% 
  setnames(c("Site_N" ,"2018_1", "2018_2")) %>% 
  .[, list(`2018_1` = ifelse(is.na(`2018_1`), 0, 1),
           `2018_2` = ifelse(is.na(`2018_2`), 0, 1)),
    by = Site_N]
S18 %>% .[, list(Site_N)] %>%
  .[!duplicated(.)] %>% full_join(S18.survey.condition, by = "Site_N") %>% View
#2018年的BBS樣區比"2018獼猴調查(樣區整理)_分析用)"完整


#=========================================
All <- rbind(S1517, S16.2,S18) %>% setDT 



