# survey results 15-18
library(here)
library(readxl)
library(data.table)
library(magrittr)
library(tidyr)
library(writexl)

# 2015-2016
S15.16 <- 
  read_xlsx("data/raw/Macaca_2015_2017_analysis.xlsx",
            sheet = 2) %>% 
  setDT %>% 
  .[Year %in% 2015:2016, 
    list(Site_N,
         Point,
         Year,
         Survey,
         Macaca_sur
    )]%>%
  .[ !duplicated(.),] 

# 2015
S15 <- 
  read_xlsx("data/raw/2015調查時段樣區內獼猴紀錄.xlsx") %>% 
  setDT %>% 
  .[, list(Site_N = `樣區編號`,
           Point = `樣點編號`,
           Year = 2015,
           Survey = `調查旅次\r\n編號`,
           Macaca_sur = ifelse(`結群` == "Y", 1, 0),
           Macaca_dist = `距離`,
           Time = `時段`)]


# 2016
S16 <- 
  read.csv("data/raw/2016樣區內獼猴調查(20161108).csv") %>% 
  setDT %>% 
  .[地點 %in% "三峽竹崙", `樣區編號` := "A05-20"] %>%
  .[, list(Site_N = `樣區編號`,
           Point = `樣點編號`,
           Year = 2016,
           Survey = `調查旅次.編號`,
           Macaca_sur = ifelse(`結群` == "Y", 1, 0),
           Macaca_dist = `距離`,
           Time = `時段`)] 
  


# 2017
S17 <- 
  read_xlsx("data/raw/2017樣區內獼猴調查.xlsx") %>% 
  setDT %>% 
  .[, list(Site_N = `樣區編號`,
           Point = `樣點編號`,
           Year = 2017,
           Survey = `調查旅次\r\n編號`,
           Macaca_sur = ifelse(`結群\r\n(修正)` == "Y", 1, 0),
           Macaca_dist = `距離`,
           Time = `時段`)]

# 2018
S18 <- 
  read_xlsx("data/raw/2018樣區內獼猴調查.xlsx") %>% 
  setDT %>% 
  .[, list(Site_N = `樣區編號`,
           Point = `樣點編號`,
           Year = 2018,
           Survey = `調查旅次\r\n編號`,
           Macaca_sur = ifelse(`結群` == "Y", 1, 0),
           Macaca_dist = `距離`,
           Time = `時段`)]

# 2019
S19 <- 
  read_xlsx("data/raw/2019樣區內獼猴調查.xlsx") %>% 
  setDT %>% 
  .[, list(Site_N = `樣區編號`,
           Point = `樣點編號`,
           Year = 2019,
           Survey = `調查旅次\r\n編號`,
           Macaca_sur = ifelse(`結群` == "Y", 1, 0),
           Macaca_dist = `距離`,
           Time = `時段`)]

S.all <- 
  rbind(S15, S16, S17, S18, S19) %>% 
  .[, Year := as.character(Year)] %>% 
  .[, Survey := as.character(Survey)] %>%
  .[, Time := as.character(Time)] %>% 
  
  .[!( Time %in% c(NA, "X","")),] %>%
  .[!( Macaca_dist %in% c(NA, "X","")),] %>% 
  .[, Point := as.numeric(Point)] 

## merge with site_info_F
site.info.F <- 
  read_xlsx("data/clean/point_Forest_1519.xlsx") %>% 
  setDT %>% 
  # 樣點向外延伸20公尺
  .[Distance_O <= 20] %>% 
  .[TypeName_O %in% c("闊葉樹林型", "竹林", "針葉樹林型", 
                        "竹闊混淆林", "針闊葉樹混淆", 
                        "竹針混淆林", "針闊葉樹混")] %>% 
  melt(id = 1:6, measure = 7:16, 
       value.name = "Do.survey",
       na.rm = TRUE) %>% 
  separate("variable", c("Year", "Survey"), "_")

all.info <- 
  S.all[site.info.F, 
        on = c("Site_N", "Point", "Year", "Survey")] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>% 
  .[Do.survey > 0] %>% 
  .[TypeName_O == "闊葉樹林型", TypeName := "純闊葉林"] %>% 
  .[TypeName_O == "竹林", TypeName := "竹林"] %>% 
  .[TypeName_O == "針葉樹林型", TypeName := "純針葉林"] %>% 
  .[TypeName_O %like% "混", TypeName := "混淆林"] %>% 
  .[!is.na(TypeName)]

S15.survey.condition <- 
  read.csv("data/raw/2015樣區內獼猴調查含樣點數(20161109).csv")  %>% 
  setDT %>% 
  .[, list(`樣點編號`, 縣市)] %>% 
  setnames(c("Site_N" ,"County"))

S16.survey.condition <- 
  read_xlsx("data/raw/2016獼猴調查(樣區整理)_分析用.xlsx",
            sheet = 1)  %>% 
  setDT %>% 
  .[, list(`樣區\r\n編號`, 縣市)] %>% 
  setnames(c("Site_N" ,"County"))

S17.survey.condition <- 
  read_xlsx("data/raw/2017獼猴調查(樣區整理)_分析用.xlsx",
            sheet = 1)  %>% 
  setDT %>% 
  .[, list(`樣區\r\n編號`, 縣市)] %>% 
  setnames(c("Site_N" ,"County"))

S18.survey.condition <- 
  read_xlsx("data/raw/2018獼猴調查(樣區整理)_分析用.xlsx",
            sheet = 1)  %>% 
  setDT %>% 
  .[, list(`樣區\r\n編號`, 縣市)] %>% 
  setnames(c("Site_N" ,"County")) 

S19.survey.condition <- 
  read_xlsx("data/raw/2019獼猴調查(樣區整理)_分析用.xlsx",
            sheet = 1)  %>% 
  setDT %>% 
  .[, list(`樣區\r\n編號`, 縣市)] %>% 
  setnames(c("Site_N" ,"County"))  


County <- rbind(S15.survey.condition,
      S16.survey.condition,
      S17.survey.condition,
      S18.survey.condition,
      S19.survey.condition) %>%
  .[ !duplicated(.),] %>% 
  .[, Site_N := as.character(Site_N)]
  

all.info %<>%   left_join(County)

write_xlsx(all.info,
           "data/clean/data_for_analysis.xlsx")

#### for summary table - survey point info

all.info %>% setDT %>% 
  .[Macaca_sur %in% c(0,1), list(Survey, TypeName, Year)] %>% table

