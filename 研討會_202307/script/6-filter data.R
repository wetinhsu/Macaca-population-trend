library(tidyverse)
library(readxl)
library(tidyr)
library(writexl)
library(sf)
library(here)


here::here() 



M.data <- read_excel(here("./研討會_202307/data/clean/full_combind_data_1522.xlsx") ) 

M.data.2 <-
  M.data %>%  
  mutate(analysis = "Y") %>% 
  mutate(analysis = ifelse(Month >= 3 & Month <= 6,  
                           analysis,
                           "N")) %>% 
#  mutate(analysis = ifelse(Altitude<50,
#                           "N",
#                           analysis))%>% 
  mutate(analysis = ifelse(is.na(X_97) & is.na(Y_97),  
                           "N",
                           analysis)) %>% 
  mutate(analysis = ifelse(as.numeric(Hour)>=11|is.na(Hour),  
                           "N",
                           analysis)) 


#-------------------
site_list <- 
  read_xlsx("//10.40.1.138/Bird Research/BBSTW (20170612)/01_調查/分層隨機取樣的樣區清單 _20221031.xlsx",
            sheet = "樣區表") 

M.data.3 <-
  M.data.2%>%
  filter(Site_N %in% site_list$樣區編號) %>% 
  filter(!(Year %in% 2020 & Survey %in% 3)) %>%
  filter(!(Site_N %in% "A09-17" & Year %in% c(2016:2019) & Point %in% c(11,12))) %>% 
  filter(analysis == "Y")

write_xlsx(M.data.3, here("./研討會_202307/data/clean/for analysis_1522_v1.xlsx"))



M.data.3 %>% .$Site_N %>% unique() %>% length
