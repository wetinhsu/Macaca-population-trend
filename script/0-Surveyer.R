#---- load library

library(data.table)
library(magrittr)
library(readxl)
library(tidyr)
library(dplyr)
library(writexl)

#------------------------
#2015

Surveyer.15 <- 
  lapply(paste0("./data/raw/BBSdata/", 2015), function(x){
    list.files(x, pattern = "BBSdata_", full.names = T) %>%  
      read_xlsx(., sheet = "birddata", cell_cols("D:AF")) %>% 
      setDT %>%
      #.[時段 %in% c("0-3minutes", "3-6minutes"),] %>%
      .[!(時段 %in% "Supplementary"),] %>%
      .[調查旅次編號 %in% c(1,2)] %>%
      .[ 分析 %in% "Y", list(年, 樣區編號, 樣點編號, 調查旅次編號, 調查者)] %>%
      setnames(., c("Year", "Site_N", "Point",  "Survey", "Surveyer")) %>%
      .[!duplicated(.)]  
  } ) %>% 
  do.call(rbind, .) %>%
  
  separate(.,Surveyer,
           into = c("Surveyer_0","Surveyer_1","Surveyer_2","Surveyer_3"),
         sep ="、", extra = "drop", fill = "right") %>% 
  reshape2::melt(.,id.vars = c("Year", "Site_N", "Point",  "Survey"),
                 variable.name = "Surveyer", value.name = "Name",) %>% 
  filter(!is.na(Name)) %>% 
  select(-Point) %>% 
  unique(.) %>%
  arrange(Year, Site_N, Survey) 


#2016

Surveyer.16 <- 
  lapply(paste0("./data/raw/BBSdata/", 2016), function(x){
    list.files(x, pattern = "BBSdata_", full.names = T) %>%  
      read_xlsx(., sheet = "birddata", cell_cols("D:AF")) %>% 
      setDT %>%
      #.[時段 %in% c("0-3minutes", "3-6minutes"),] %>%
      .[!(時段 %in% "Supplementary"),] %>%
      .[調查旅次編號 %in% c(1,2)] %>%
      .[ 分析 %in% "Y", list(年, 樣區編號, 樣點編號, 調查旅次編號, 調查者)] %>%
      setnames(., c("Year", "Site_N", "Point",  "Survey", "Surveyer")) %>%
      .[!duplicated(.)]  
  } ) %>% 
  do.call(rbind, .)%>%
  
  separate(.,Surveyer,
           into = paste0("Surveyer","_",0:10),
           sep ="、", extra = "drop", fill = "right") %>% 
  reshape2::melt(.,id.vars = c("Year", "Site_N", "Point",  "Survey"),
                 variable.name = "Surveyer", value.name = "Name",) %>% 
  filter(!is.na(Name)) %>% 
  select(-Point) %>% 
  unique(.) %>%
  arrange(Year, Site_N, Survey) 

  
#2017

Surveyer.17 <- 
  lapply(paste0("./data/raw/BBSdata/", 2017), function(x){
    list.files(x, pattern = "BBSdata_", full.names = T) %>%  
      read_xlsx(., sheet = "birddata", cell_cols("D:AF")) %>% 
      setDT %>%
      #.[時段 %in% c("0-3minutes", "3-6minutes"),] %>%
      .[!(時段 %in% "Supplementary"),] %>%
      .[調查旅次編號 %in% c(1,2)] %>%
      .[ 分析 %in% "Y", list(年, 樣區編號, 樣點編號, 調查旅次編號, 調查者)] %>%
      setnames(., c("Year", "Site_N", "Point",  "Survey", "Surveyer")) %>%
      .[!duplicated(.)]  
  } ) %>% 
  do.call(rbind, .)%>%
  
  separate(.,Surveyer,
           into = paste0("Surveyer","_",0:10),
           sep ="、", extra = "drop", fill = "right") %>% 
  reshape2::melt(.,id.vars = c("Year", "Site_N", "Point",  "Survey"),
                 variable.name = "Surveyer", value.name = "Name",) %>% 
  filter(!is.na(Name)) %>% 
  select(-Point) %>% 
  unique(.) %>%
  arrange(Year, Site_N, Survey) 


#2018

Surveyer.18<- 
  lapply(paste0("./data/raw/"), function(x){
    list.files(x, pattern = "BBSdata_2018", full.names = T) %>%  
      read_xlsx(., sheet = "birddata", cell_cols("D:AF")) %>% 
      setDT %>%
      .[時段 %in% c("A", "B","a","b","NA"),] %>%
      #.[!(時段 %in% "Supplementary"),] %>%
      .[調查旅次編號 %in% c(1,2)] %>%
      #.[ 分析 %in% "Y",] %>%
      .[, list(年, 樣區編號, 樣點編號, 調查旅次編號, 調查者)] %>%
      setnames(.,  c("Year", "Site_N", "Point",  "Survey", "Surveyer")) %>%
      .[!duplicated(.)]  
  } ) %>% 
  do.call(rbind, .) %>% 
  
  separate(.,Surveyer,
           into = paste0("Surveyer","_",0:10),
           sep = c("[:punct:]"), extra = "drop", fill = "right") %>% 

  reshape2::melt(.,id.vars = c("Year", "Site_N", "Point",  "Survey"),
                 variable.name = "Surveyer", value.name = "Name",) %>% 
  filter(!is.na(Name)) %>% 
  select(-Point) %>% 
  unique(.) %>%
  arrange(Year, Site_N, Survey) 




#2019
Surveyer.19 <- 
  lapply(paste0("./data/raw/"), function(x){
    list.files(x, pattern = "BBSdata_2019", full.names = T) %>%  
      read_xlsx(., sheet = "birddata", cell_cols("D:AF")) %>% 
      setDT %>%
      #.[時段 %in% c("0-3minutes", "3-6minutes"),] %>%
      .[!(時段 %in% "Supplementary"),] %>%
      .[調查旅次編號 %in% c(1,2)] %>%
      #.[ 分析 %in% "Y",] %>% 
      .[, list(年, 樣區編號, 樣點編號, 調查旅次編號, 調查者)] %>%
      setnames(.,  c("Year", "Site_N", "Point",  "Survey", "Surveyer")) %>%
      .[!duplicated(.)]  
  } ) %>% 
  do.call(rbind, .)%>% 
  
  separate(.,Surveyer,
           into = paste0("Surveyer","_",0:10),
           sep = c("[:punct:]"), extra = "drop", fill = "right") %>% 
  
  reshape2::melt(.,id.vars = c("Year", "Site_N", "Point",  "Survey"),
                 variable.name = "Surveyer", value.name = "Name",) %>% 
  filter(!is.na(Name)) %>% 
  select(-Point) %>% 
  unique(.) %>%
  arrange(Year, Site_N, Survey) 
