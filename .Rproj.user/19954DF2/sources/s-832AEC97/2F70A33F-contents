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
    )]
# 2017
S17 <- 
  read_xlsx("data/raw/2017樣區內獼猴調查.xlsx") %>% 
  setDT %>% 
  .[, list(Site_N = `樣區編號`,
           Point = `樣點編號`,
           Year = 2017,
           Survey = `調查旅次\r\n編號`,
           Macaca_sur = ifelse(`結群\r\n(修正)` == "Y", 1, 0))]

# 2018
S18 <- 
  read_xlsx("data/raw/2018樣區內獼猴調查.xlsx") %>% 
  setDT %>% 
  .[, list(Site_N = `樣區編號`,
           Point = `樣點編號`,
           Year = 2018,
           Survey = `調查旅次\r\n編號`,
           Macaca_sur = ifelse(`結群` == "Y", 1, 0))]

S.all <- 
  rbind(S15.16, S17, S18) %>% 
  .[, Year := as.character(Year)] %>% 
  .[, Survey := as.character(Survey)]

## merge with site_info_F
site.info.F <- 
  read_xlsx("data/clean/point_Forest_1518.xlsx") %>% 
  setDT %>% 
  # 樣點向外延伸20公尺
  .[Distance_O <= 20] %>% 
  .[TypeName_O.x %in% c("闊葉樹林型", "竹林", "針葉樹林型", 
                        "竹闊混淆林", "針闊葉樹混淆", 
                        "竹針混淆林", "針闊葉樹混")] %>% 
  melt(id = 1:6, measure = 7:14, 
       value.name = "Do.survey",
       na.rm = TRUE) %>% 
  separate("variable", c("Year", "Survey"), "_")

all.info <- 
  S.all[site.info.F, 
        on = c("Site_N", "Point", "Year", "Survey")] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>% 
  .[Do.survey > 0] %>% 
  .[TypeName_O.x == "闊葉樹林型", TypeName := "純闊葉林"] %>% 
  .[TypeName_O.x == "竹林", TypeName := "竹林"] %>% 
  .[TypeName_O.x == "針葉樹林型", TypeName := "純針葉林"] %>% 
  .[TypeName_O.x %like% "混", TypeName := "混淆林"] %>% 
  .[!is.na(TypeName)]
  

write_xlsx(all.info,
           "data/clean/data_for_analysis.xlsx")

#### for summary table - survey point info
point.count <- 
  read_xlsx("data/clean/point_Forest_1518.xlsx") %>% 
  setDT %>% 
  melt(id = 1:6, measure = 7:14, 
       value.name = "Do.survey",
       na.rm = TRUE) %>% 
  separate("variable", c("Year", "Survey"), "_") %>% 
  .[, .N, by = list(Year, Survey)]
