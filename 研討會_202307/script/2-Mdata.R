# survey results 15-21
library(here)
library(readxl)
library(tidyverse)
library(writexl)

M1521 <- 
  read_xlsx("研討會_202307/data/raw/歷年獼猴資料整理_20230119明剛.xlsx",
            sheet = "工作表1")%>% 
  filter(年 %in%  2015:2021) %>% 
  mutate(Macaca_sur = ifelse(`結群` == "Y", 1, 0)) %>% 
  
  select(Site_N = `樣區編號`,
           Point = `樣點編號`,
           Year = 年,
           Survey = `調查旅次編號`,
           Macaca_sur,
           Macaca_dist = `距離`,
         Time = `時段`)%>%
  unique() %>% 
  
  mutate(Time = case_when(
    Time  %in% "0-6minutes"~ "AB",
    Time  %in% "0-3minutes"~ "A",
    Time  %in% "3-6minutes"~ "B",
    Time  %in% "Supplementary"~ "X"
  )) %>% 
  
  mutate(Macaca_dist = case_when(
    Macaca_dist %in% "0-25m"~ "A",
    Macaca_dist %in% "25-100m"~ "B",
    Macaca_dist %in% ">100m"~ "C"
  ))





M.all <- 
  M1521 %>% 
  mutate_at(c("Year", "Survey", "Time", "Site_N"), as.character) %>% 
  filter( Time %in% c("A","B","AB") ) %>% 
  filter( Macaca_dist %in% c("A","B","C") ) %>%
  mutate(Point =  Point %>% as.character %>% as.numeric) 


M.all  %>% 
  group_by(Year, Site_N, Point, Survey) %>% 
  summarise(N = n())%>% View

write_xlsx(M.all, "研討會_202307/data/clean/Macaca_1521_v1.xlsx")

