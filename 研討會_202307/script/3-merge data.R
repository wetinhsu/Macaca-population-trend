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

M.data <- M.all %>% 
  full_join(S.all, by = c("Year", "Site_N", "Point", "Survey")) %>% 
  
  filter(! str_detect(Site_N, "KIN")) %>% #exculde kiman
  filter(!(Site_N %in%  paste0("A08-0", 1:9) )) %>% #exclude蘭嶼
  filter(! is.na(X_97)) %>%
  arrange(Year,Site_N)

M.data %>%  #確認每一旅次內的每一個樣點資料只有1筆
  group_by(Year, Survey, Site_N, Point) %>% 
  summarise( n=n()) %>% 
  filter(n >1) 

write_xlsx(M.data, "./研討會_202307/data/clean/merge_data_1522.xlsx")
