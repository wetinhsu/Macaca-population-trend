#---- load library
library(readxl)
library(writexl)
library(tidyverse)
library(sf)

#-----------------------------
#read point data

S.all<- "D:/R/test/Macaca-population-trend/研討會_202307/data/clean/Site_1521_v1.xlsx" %>%
  read_xlsx(., sheet = 1, cell_cols("A:K"), col_types = "text")%>% 
  mutate(Year = as.character(Year))%>% 
  mutate_at(c("X", "Y"), as.numeric)  

#transform to spatial data
st.all<- 
  S.all %>% 
  filter(!is.na(X)|!is.na(Y)) %>% 
  dplyr::select(X, Y) %>% 
  unique %>% 
  mutate(NO = 1 : nrow(.)) %>% 
  mutate_at(c("X", "Y"), as.numeric)





#--------------------------------
#combind distance to point data

S.all <- 
st.all %>%
  
  left_join(S.all, .,  by = c("X", "Y")) %>% 
  dplyr::select(-NO)


M.all <- read_excel("./研討會_202307/data/clean/Macaca_1521_v1.xlsx", col_types = "text")

M.data<- M.all %>% 
  full_join(S.all, by = c("Year", "Site_N", "Point", "Survey")) %>% 
  mutate_at(c("X", "Y"), as.numeric) %>% 
  arrange(Year,Site_N)

M.data %>%  #確認每一旅次內的每一個樣點資料只有1筆
  group_by(Year, Survey, Site_N, Point) %>% 
  summarise( n=n()) %>% 
  filter(n >1) 

write_xlsx(M.data, "./研討會_202307/data/clean/merge_data_1521.xlsx")
