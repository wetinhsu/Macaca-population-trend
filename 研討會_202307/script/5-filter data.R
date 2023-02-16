library(tidyverse)
library(readxl)
library(tidyr)
library(writexl)
library(sf)
library(here)


here::here() 



M.data <- read_excel(here("./研討會_202307/data/clean/full_combind_data_1521.xlsx") ) %>% 
  mutate(Macaca_sur = ifelse(Macaca_sur %in% 1 & Macaca_dist %in% "C" , NA, Macaca_sur)) 




M.data.1 <- 
  M.data %>%
  mutate(DATE = paste(Year, Month, Day, sep = "-") %>% as.Date()) %>% 
  mutate(julian.D = format(DATE, "%j") %>% as.numeric())

M.data.2 <-
  M.data.1 %>%  
  mutate(analysis = "Y") %>% 
  mutate(analysis = ifelse(Month >= 3 & Month <= 6,  
                           analysis,
                           "N")) %>% 
  mutate(analysis = ifelse(Altitude<50,
                           "N",
                           analysis))%>% 
  mutate(analysis = ifelse(is.na(X) & is.na(Y),  
                           "N",
                           analysis)) %>% 
  mutate(analysis = ifelse(as.numeric(Hour)>=11,  
                           "N",
                           analysis)) 


write_xlsx(M.data.2, here("./研討會_202307/data/clean/for analysis_1521.xlsx"))



