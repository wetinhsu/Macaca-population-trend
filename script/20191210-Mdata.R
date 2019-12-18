# survey results 15-18
library(here)
library(readxl)
library(data.table)
library(magrittr)
library(tidyr)
library(dplyr)
library(writexl)


# 2015
M15 <- 
  read_xlsx("data/raw/2015調查時段樣區內獼猴紀錄.xlsx",
            sheet = 3) %>% 
  setDT %>% 
  .[, list(Site_N = `樣區編號`,
           Point = `樣點編號`,
           Year = 2015,
           Survey = `調查旅次\r\n編號`,
           Macaca_sur = ifelse(`結群` == "Y", 1, 0),
           Macaca_dist = `距離`,
           Time = `時段`)]%>% setDT %>% .[!duplicated(.)]


# 2016
M16 <- 
  read_xlsx("data/raw/2016樣區內獼猴調查.xlsx",
            sheet = 2) %>% 
  setDT %>% 
  .[地點 %in% "三峽竹崙", `樣區編號` := "A05-20"] %>%
  .[, list(Site_N = `樣區編號`,
           Point = `樣點編號`,
           Year = 2016,
           Survey = `調查旅次\r\n編號`,
           Macaca_sur = ifelse(`結群` == "Y", 1, 0),
           Macaca_dist = `距離`,
           Time = `時段`)] %>% setDT %>% .[!duplicated(.)]



# 2017
M17 <- 
  read_xlsx("data/raw/2017樣區內獼猴調查.xlsx") %>% 
  setDT %>% 
  .[, list(Site_N = `樣區編號`,
           Point = `樣點編號`,
           Year = 2017,
           Survey = `調查旅次\r\n編號`,
           Macaca_sur = ifelse(`結群\r\n(修正)` == "Y", 1, 0),
           Macaca_dist = `距離`,
           Time = `時段`)]%>% setDT %>% .[!duplicated(.)]

# 2018
M18 <- 
  read_xlsx("data/raw/2018樣區內獼猴調查.xlsx") %>% 
  setDT %>% 
  .[, list(Site_N = `樣區編號`,
           Point = `樣點編號`,
           Year = 2018,
           Survey = `調查旅次\r\n編號`,
           Macaca_sur = ifelse(`結群` == "Y", 1, 0),
           Macaca_dist = `距離`,
           Time = `時段`)]%>% setDT %>% .[!duplicated(.)]

# 2019
M19 <- 
  read_xlsx("data/raw/2019樣區內獼猴調查.xlsx") %>% 
  setDT %>% 
  .[, list(Site_N = `樣區編號`,
           Point = `樣點編號`,
           Year = 2019,
           Survey = `調查旅次\r\n編號`,
           Macaca_sur = ifelse(`結群` == "Y", 1, 0),
           Macaca_dist = `距離`,
           Time = `時段`)]%>% setDT %>% .[!duplicated(.)]

M.all <- 
  rbind(M15, M16, M17, M18, M19) %>% 
  .[, Year := as.character(Year)] %>% 
  .[, Survey := as.character(Survey)] %>%
  .[, Time := as.character(Time)] %>% 
  
  .[ Time %in% c("A","B"),] %>%
  .[( Macaca_dist %in% c("A","B","C")),] %>% 
  .[, Point := as.numeric(Point)] %>% 
  .[, Site_N := as.character(Site_N)]

M.all  %>% setDT %>%.[, .N, by =  c("Year", "Site_N", "Point", "Survey")] %>% View

write_xlsx(M.all, "data/clean/MAcaca/Macaca_1519_v1.xlsx")
