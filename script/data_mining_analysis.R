# survey results 15-18
library(here)
library(readxl)
library(data.table)
library(magrittr)
library(tidyr)
library(dplyr)
library(writexl)


# 2015
S15 <- 
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
           Time = `時段`)] %>% setDT %>% .[!duplicated(.)]
  


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
           Time = `時段`)]%>% setDT %>% .[!duplicated(.)]

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
           Time = `時段`)]%>% setDT %>% .[!duplicated(.)]

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
           Time = `時段`)]%>% setDT %>% .[!duplicated(.)]

S.all <- 
  rbind(S15, S16, S17, S18, S19) %>% 
  .[, Year := as.character(Year)] %>% 
  .[, Survey := as.character(Survey)] %>%
  .[, Time := as.character(Time)] %>% 
  
  .[ Time %in% c("A","B"),] %>%
  .[( Macaca_dist %in% c("A","B","C")),] %>% 
  .[, Point := as.numeric(Point)] %>% 
  .[, Site_N := as.character(Site_N)] 


remove.data <- 
  rbind(S15, S16, S17, S18, S19) %>% 
  .[, Year := as.character(Year)] %>% 
  .[, Survey := as.character(Survey)] %>%
  .[, Time := as.character(Time)] %>% 
  
  .[!( Time %in% c("A","B")) |!( Macaca_dist %in%c("A","B","C")),] %>% 
  .[, Site_N := as.character(Site_N)] %>%
  .[order(Year, Site_N, Point, Survey),]

remove.data %>% setDT %>% 
  .[, list(Survey, Year)] %>% table

S.all %>% rbind(.,remove.data) %>% setDT %>% 
  .[, list(Survey, Year)] %>% table 

remove.data %>% setDT %>% 
  .[Macaca_sur %in% 1, list(Survey, Year)] %>% table 


S.all %>% setDT %>% 
  .[Macaca_sur %in% 1, list(Survey, Year)] %>% table


S.all %>% setDT %>%  .[!(Macaca_dist %in% "C"),] %>%
  .[Macaca_sur %in% 1, list(Survey, Year)] %>% table

County <-
  read_xlsx("data/raw/all point_20191023.xlsx",
            sheet = 1)  %>% 
  setDT %>% 
  .[, list(`樣區編號`, 縣市)] %>% 
  setnames(c("Site_N" ,"County")) %>%
  .[, Site_N := as.character(Site_N)] %>%
  .[!duplicated(.)]  %>% 
  setDT 


sum.tab.group<- S.all  %>%   left_join(County, by = "Site_N") %>% setDT %>% 
  .[, .( N = .N,
         Site = length(unique(Site_N)),
         point = length(unique(paste0(Site_N,"-0",Point)))),
   by = list( Macaca_sur, Year, County)] %>% setDT %>%
  .[order(Macaca_sur,Year, County),] %>%
  .[Macaca_sur %in% 1,] %>%
  dcast(., County ~ Year, value.var = c("N", "Site", "point"))

sum.tab.single<- S.all  %>%   left_join(County, by = "Site_N") %>% setDT %>% 
  .[., .( N = .N,
         Site = length(unique(Site_N)),
         point = length(unique(paste0(Site_N,"-0",Point)))),
    by = list( Macaca_sur, Year, County)] %>% setDT %>%
  .[order(Macaca_sur,Year, County),] %>%
  .[Macaca_sur %in% 0,] %>%
  dcast(., County ~ Year, value.var = c("N", "Site", "point")) 

write_xlsx(list("Group" = sum.tab.group,
                "Single" = sum.tab.single),
                "Results/sum_table_1028.xlsx")


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
  .[!is.na(TypeName)] %>%
  .[, Site_N := as.character(Site_N)] %>% setDT

remove.data.2 <- all.info %>% setDF%>%
  anti_join(S.all, ., by = c("Site_N", "Point", "Year", "Survey")) %>%
  left_join(all.info) 


County <-
  read_xlsx("data/raw/all point_20191023.xlsx",
            sheet = 1)  %>% 
  setDT %>% 
  .[, list(`樣區編號`, 縣市)] %>% 
  setnames(c("Site_N" ,"County")) %>%
  .[!duplicated(.)]  %>% 
  setDT 


all.info.1 <- all.info %>%   left_join(County, by = "Site_N") %>% setDT 

write_xlsx(all.info.1,
           "data/clean/data_for_analysis.xlsx")

#### for summary table - survey point info

all.info.1 %>% setDT %>% 
  .[, list(Survey, TypeName,Year)] %>% table




