library(readxl)
library(tidyverse)
library(writexl)

#------------------
site_list <- 
  read_xlsx("//10.40.1.138/Bird Research/BBSTW (20170612)/01_調查/分層隨機取樣的樣區清單 _20221031.xlsx",
            sheet = "樣區表") 


#-----------------------------
#read point data


df <- read_excel("研討會_202307/有林務局資料/data/clean/for analysis_1522_v1.xlsx", col_types = "text") 
  
#ifelse(`結群` == "Y", 1, 0)


#----------------------------------

site_info <- 
  df %>% 
  filter(Year %in% 2015:2022) %>% 
  select(Year, Site_N, Point, Survey) %>% 
  unique() %>% 
  group_by(Year, Site_N) %>% 
  summarise(point_N = n()) 

spieces_info <-
  df %>% 
  filter(Macaca_sur ==1)%>%
  filter(Year %in% 2015:2022) %>% 
  filter(analysis %in% "Y") %>% 
  filter(!(Year %in% 2020 & Survey %in% 3)) %>% 
  
  group_by(Year, Site_N, Survey) %>% 
  summarise(NN = sum(as.numeric(Macaca_sur), na.rm = T)) %>% 
  group_by(Year, Site_N) %>% 
  summarise(NN_max = max(NN, na.rm = T)) %>% 
  
  
  
      full_join(site_info, ., by = c("Year", "Site_N")) %>% 
      filter(Site_N %in% site_list$樣區編號) %>% 
  
  mutate(Count = ifelse(is.na(NN_max),
                        0,
                        NN_max) ) %>% 
      
      reshape2::dcast( Site_N ~ Year, value.var = "Count") %>% 
      arrange(Site_N) %>% 
      setNames(., str_replace_all(colnames(.), "Site_N", "site"))


 write.csv(spieces_info,
           "./研討會_202307/有林務局資料/data/clean/BBS_Monkey_1522_0710.csv",
           row.names = F)


spieces_info %>% 
  reshape2::melt(id = "site", variable.name = "year", value.name = "count") %>% 
  reshape2::dcast(count~year, length)

