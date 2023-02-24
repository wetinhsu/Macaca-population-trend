library(data.table)
library(tidyverse)
library(readxl)
library(writexl)
library(here)
#---------------------------


df <- read_excel("研討會_202307/data/clean/for analysis_1521_v2.xlsx", col_types = "text") 

df%>%
  summarise(
    length = Site_N %>% unique() %>% length
  ) 


df%>%
group_by(Year,  Survey) %>% 
  summarise(
    n_Site_N = Site_N %>% unique() %>% length,
  ) %>% 
  summary()


df%>%
  filter(Macaca_sur == 1) %>% 
  group_by(Year, Site_N, Survey) %>% 
  summarise(NN = Macaca_sur %>% length) %>% 
  group_by(Year, Survey) %>%
  summarise(NN_survey = NN %>% as.numeric() %>% sum) %>% 
  summary


df%>%
  filter(Macaca_sur == 1) %>% 
  summarise(NN = Macaca_sur %>% length)

#-------------------
mmm <- 
df%>%
  filter(Macaca_sur == 1) %>% 
  filter(!(Year %in% 2020 & Survey %in% 3)) %>% 
  select(Site_N, Year,Survey) %>% 
  unique() %>% 
  reshape2::dcast(Site_N ~ Year, length) %>% 
  mutate(total = `2015`+`2016`+`2017`+`2018`+`2019`+`2020`+`2021`) 


mmm$total %>% table #猴子的聚集地A33-08、A18-01 
#其實不是我們想像中出現猴群的就是那幾個樣區

#-------------
site_list <- 
  read_xlsx("//10.40.1.138/Bird Research/BBSTW (20170612)/01_調查/分層隨機取樣的樣區清單 _20221031.xlsx",
            sheet = "樣區表") 

source(here("./研討會_202307/script/1-Sitedata.R"))
Surveyer <- 
dfo %>% 
  filter(年 %in% 2015:2021) %>% 
  filter(!(年 %in% 2020 & 調查旅次編號 %in% 3)) %>% 
  select(年, 樣區編號,樣點編號, 調查旅次編號,調查者) %>% 
  unique() %>%
  
  separate(.,調查者,
           into = paste0("Surveyer","_",0:10),
           sep ="、", extra = "drop", fill = "right") %>% 
  reshape2::melt(.,id.vars = c("年", "樣區編號", "樣點編號",  "調查旅次編號"),
                 variable.name = "Surveyer", value.name = "Name",) %>% 
  filter(!is.na(Name)) %>% 
  select(-樣點編號) %>% 
  unique(.) 



Surveyer %>% 
  filter(樣區編號 %in% df$Site_N) %>% 
  
  group_by(年,調查旅次編號) %>% 
  summarise(NN=Name %>% unique() %>% length) %>% 
  summary

#----------------------------------------------------------------

df %>% 
  group_by(Year, Site_N, Survey) %>% 
  summarise(NN = length(Point)) %>% 
  .$NN %>% 
  table


df %>% 
  group_by(Year, Site_N, Survey) %>% 
  summarise(NN = Site_N %>% unique()%>% length() ) %>% 
  group_by(Site_N) %>% 
  summarise(Count = NN %>% length() ) %>% 
  
  .$Count %>%  
    table
#-------------------------------------------------


dfo %>% 
  select(年, 樣區編號) %>% 
  setnames(., c("Year", "Site_N")) %>% 
  filter(Site_N %in% site_list$樣區編號) %>%
  
  filter(Year %in% c(2015:2021)) %>% 
  unique() %>%
  .$Site_N %>% unique() %>%length

group_by(Site_N) %>% 
  summarise(Count= n()) %>% 
  .$Count %>%table


dfo %>% 
  select(年, 樣區編號) %>% 
  setnames(., c("Year", "Site_N")) %>% 
  filter(Year %in% c(2011:2019)) %>% 
  filter(Site_N %in% site_list$樣區編號) %>%
  
  
  unique() %>% 
  group_by(Site_N) %>% 
  summarise(Count=  length(Year) ) %>% View
  filter(Count>=3)
  
  