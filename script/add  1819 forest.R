
#---- load library

library(data.table)
library(magrittr)
library(readxl)
library(tidyr)
library(purrr)
library(dplyr)
library(writexl)

S1517 <- read_excel("data/clean/point_Forest_1517.xlsx") %>% setDT%>% 
  .[, Point := as.numeric(Point)]%>% 
  .[, Site_N := as.character(Site_N)]





# 2018
S18 <- 
  read_xlsx("data/raw/2018獼猴調查(樣區整理)_分析用.xlsx",
            sheet = 2,
            col_types = "text") %>% 
  setDT %>% 
  .[, list(`樣點編號`,
           X_2018 = as.numeric(WGS84_X...10),
           Y_2018 = as.numeric(WGS84_Y...11))] %>% 
  separate("樣點編號", c("Site_a", "Site_b","Point"), "-") %>% setDT %>%
  .[, Site_N := paste(Site_a, Site_b, sep = "-")]  %>%
  .[, c("Site_a", "Site_b") := NULL] %>% 
  .[, Point := as.numeric(Point)]
S18.survey.condition <- 
  read_xlsx("data/raw/2018獼猴調查(樣區整理)_分析用.xlsx",
            sheet = 1) %>% 
  setDT %>% 
  .[, list(`樣區\r\n編號`, 第一旅次, 第二旅次)] %>% 
  setnames(c("Site_N" ,"2018_1", "2018_2")) %>% 
  .[, list(`2018_1` = ifelse(is.na(`2018_1`), 0, 1),
           `2018_2` = ifelse(is.na(`2018_2`), 0, 1)),
    by = Site_N]

S18 %<>% S18.survey.condition[., on = "Site_N"]   %>%
  setDT %>% 
  .[, Point := as.numeric(Point)]%>% 
  .[, Site_N := as.character(Site_N)]


# 2019
S19 <- 
  read_xlsx("data/raw/2019獼猴調查(樣區整理)_分析用.xlsx",
            sheet = 2,
            col_types = "text") %>% 
  setDT %>% 
  .[, list(`樣點編號`,
           X_2019 = as.numeric(WGS84_X...10),
           Y_2019 = as.numeric(WGS84_Y...11))] %>% 
  separate("樣點編號", c("Site_a", "Site_b","Point"), "-") %>% setDT %>%
  .[, Site_N := paste(Site_a, Site_b, sep = "-")]  %>%
  .[, c("Site_a", "Site_b") := NULL] %>% 
  .[, Point := as.numeric(Point)]
S19.survey.condition <- 
  read_xlsx("data/raw/2019獼猴調查(樣區整理)_分析用.xlsx",
            sheet = 1) %>% 
  setDT %>% 
  .[, list(`樣區\r\n編號`, 第一旅次, 第二旅次)] %>% 
  setnames(c("Site_N" ,"2019_1", "2019_2")) %>% 
  .[, list(`2019_1` = ifelse(is.na(`2019_1`), 0, 1),
           `2019_2` = ifelse(is.na(`2019_2`), 0, 1)),
    by = Site_N]

S19 %<>% S19.survey.condition[., on = "Site_N"]   %>%
  setDT %>% 
  .[, Point := as.numeric(Point)]%>% 
  .[, Site_N := as.character(Site_N)]


aa <- read_excel("data/clean/point_Forest_1519.xlsx") %>% setDT%>% 
  .[, Point := as.numeric(Point)]%>% 
  .[, Site_N := as.character(Site_N)] %>%
  .[, list(Site_N, Point, TypeName = TypeName_O,
           Distance = Distance_O)]

bb <- read_excel("data/clean/point_Forest_1519.xlsx") %>% setDT%>% 
  .[, Point := as.numeric(Point)]%>% 
  .[, Site_N := as.character(Site_N)] %>%
  .[, list(Site_N, Point, TypeName = TypeName_O,
           Distance = Distance_O)] %>%
  .[duplicated(., by =  c("Site_N", "Point"))] %>%
  .[,list(Site_N, Point)]

left_join(bb,aa)

aa <- read_excel("data/clean/point_Forest_1519.xlsx") %>% setDT%>% 
  .[, Point := as.numeric(Point)]%>% 
  .[, Site_N := as.character(Site_N)] %>%
  .[, list(Site_N, Point, TypeName = TypeName_O,
           Distance = Distance_O)]%>%
  .[!duplicated(., by =  c("Site_N", "Point"))]


full<- S1517 %>% full_join(S18, by = c("Site_N", "Point")) %>%
  full_join(S19, by = c("Site_N", "Point")) %>%
  setDT %>%
  left_join(aa, by = c("Site_N", "Point"), suffix = c("", ".y"))  %>%
  setDT %>%
  .[is.na(TypeName), TypeName := TypeName.y] %>%
  .[is.na(Distance), Distance := Distance.y] %>%
  .[,c("X_2018","Y_2018", "X_2019","Y_2019","TypeName.y","Distance.y") := NULL]

full %>% setDT %>% .[duplicated(., by =  c("Site_N", "Point"))] 


write_xlsx(full,"data/clean/point_Forest_1519(20191031).xlsx")

