# survey results 15-24
library(here)
library(readxl)
library(tidyverse)
library(writexl)

#-----------------------------------------------------------


dfo  <- data.table::fread("../bbs_handover_temp_v20190123 - WT/dfs2.csv",
                          head=T, encoding = "UTF-8", na.strings = "")   #UTF-8
dfo  <-
dfo %>% 
  filter(!(年== 2017& DataID %in% c(60562, 60567, 54106)))%>%  
  filter(!(年== 2022& DataID %in% c(994,41921, 56283))) %>% # 2022的56283對面山頭的猴群
  filter(!(年== 2024& DataID %in% c(994,41921))) %>% 
  filter(!(年== 2016& DataID %in% c(49383,49384)))

M1524 <- 
  dfo%>% 
  filter(species_nr == 841) %>% 
  filter(年 %in%  2015:2024) %>% 
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




#---------------------
# M22<- 
#   read_xlsx("./data/raw/2022樣區內獼猴調查.xlsx",
#             sheet = "工作表1", col_types = "text")%>% 
#   mutate(Macaca_sur = ifelse(`結群` == "Y", 1, 0)) %>% 
#   
#   select(Site_N = `樣區編號`,
#          Point = `樣點編號`,
#          Year = 年,
#          Survey = `調查旅次編號`,
#          Macaca_sur,
#          Macaca_dist = `距離`,
#          Time = `時段`)%>%
#   unique() %>% 
#   
#   mutate(Time = case_when(
#     Time  %in% "0-6minutes"~ "AB",
#     Time  %in% "0-3minutes"~ "A",
#     Time  %in% "3-6minutes"~ "B",
#     Time  %in% "Supplementary"~ "X"
#   )) %>% 
#   
#   mutate(Macaca_dist = case_when(
#     Macaca_dist %in% "0-25m"~ "A",
#     Macaca_dist %in% "25-100m"~ "B",
#     Macaca_dist %in% ">100m"~ "C"
#   ))
# 


#-----------------------------------------------------------
M1523 <- 
  read_xlsx("./data/raw/歷年獼猴資料整理_20241022明剛.xlsx",
            sheet = "工作表1", col_types = "text")%>% 
  filter(年 %in%  2015:2023) %>% 
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



#---------------------

M.all <- 
  M1524 %>%
  mutate_at(c("Year", "Survey", "Time", "Site_N"), as.character) %>% 
  filter( Time %in% c("A","B","AB") ) %>% 
  filter( Macaca_dist %in% c("A","B","C") ) %>%
  mutate(Point =  Point %>% as.character %>% as.numeric) 


M.all  %>% 
  group_by(Year, Site_N, Point, Survey) %>% 
  summarise(N = n())%>% View


write_xlsx(M.all, "data/clean/Macaca/Macaca_1524_v1.xlsx")
