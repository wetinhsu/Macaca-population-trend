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
           Time = `時段`)] %>% setDT %>% .[!duplicated(.)]%>%
  .[  !(Year %in% 2016 & Site_N %in% "A19-01" & Point %in% 2 & Survey %in% 1 &　Macaca_sur %in% 0), ] %>%
  .[  !(Year %in% 2016 & Site_N %in% "A19-01" & Point %in% 5 & Survey %in% 2 &　Macaca_sur %in% 0), ] %>%
  .[  !(Year %in% 2016 & Site_N %in% "A19-01" & Point %in% 5 & Survey %in% 2 &　Macaca_sur %in% 1 & Macaca_dist %in% "B"), ] 

  


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
           Time = `時段`)]%>% setDT %>% .[!duplicated(.)] %>% 
.[  !(Year %in% 2018 & Site_N %in% "A05-21" & Point %in% 2 & Survey %in% 2 &　Macaca_dist %in% "B"), ]

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


forest.1519<- read_excel("data/clean/point_Forest_1519(20191031).xlsx") %>%
  setDT %>%
  .[Site_N %in%  "C14-01", `2016_1` := 1] %>%
  .[Site_N %in%  "C14-01", `2016_2` := 1] %>%
  .[Site_N %in%  "C14-02", `2016_1` := 1] %>%
  .[Site_N %in%  "C14-02", `2016_2` := 1] %>%
  .[Site_N %in%  "A16-01", `2016_1` := 1] %>%
  .[Site_N %in%  "A16-01", `2016_2` := 1] %>%
  .[Site_N %in%  "A17-01", `2016_2` := 1] %>%
  .[Site_N %in%  "A09-20", `2016_2` := 1] %>%
  melt(., id =  1:4) %>%
  .[, Year := substr(variable,1,4)] %>% setDT %>%
  .[, Survey := as.character(substr(variable,6,6))] %>%
  .[, Do.survey := value] %>%
  .[, c("variable","value"):= NULL] %>%
  .[ !is.na(Do.survey),]%>% 
  .[!duplicated(., by = c("Site_N", "Point", "Year", "Survey"))]



point.list <- read_xlsx("data/raw/all point_20191023.xlsx",
                        sheet = 1)  %>% 
  setDT %>% 
  .[!duplicated(.)]  %>% 
  setDT %>%
  setnames(., c("Site_N", "Point", "County", "Name", "Pointid",
                "X.67", "Y.67", "X.97", "Y.97", "X.dms", "Y.dms", "X.84", "Y.84",
                "Note1", "Note2"))%>%
  .[, Site_N := as.character(Site_N)] %>%
  .[, Point := as.numeric(Point)] %>%
  .[, list(Site_N,  County)] %>%.[!duplicated(.)]



S.all <- 
  rbind(S15, S16, S17, S18, S19) %>% 
  .[, Year := as.character(Year)] %>% 
  .[, Survey := as.character(Survey)] %>%
  .[, Time := as.character(Time)] %>% 
  
  .[ Time %in% c("A","B"),] %>%
  .[( Macaca_dist %in% c("A","B","C")),] %>% 
  .[, Point := as.numeric(Point)] %>% 
  .[, Site_N := as.character(Site_N)]  %>%
  
  full_join(forest.1519) %>%
  setDT %>%
  .[!(Do.survey %in% 0),] %>% #有做調查的所有樣點 
  setDT %>%
  .[TypeName %like% "混", TypeName.1 := "混淆林"] %>%
  .[TypeName %like% "針葉樹", TypeName.1 := "針葉林"] %>%
  .[TypeName %like% "闊葉樹", TypeName.1 := "闊葉林"] %>%
  .[TypeName %like% "竹林", TypeName.1 := "竹林"] %>%
  .[, Altitude := as.character(substr(Site_N,1,1))] %>%
  left_join(point.list) %>% setDT





write_xlsx(S.all ,"data/clean/data_for_analysis_1519.xlsx")






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
  .[, .( N = .N,
         Site = length(unique(Site_N)),
         point = length(unique(paste0(Site_N,"-0",Point)))),
    by = list( Macaca_sur, Year, County)] %>% setDT %>%
  .[order(Macaca_sur,Year, County),] %>%
  .[Macaca_sur %in% 0,] %>%
  dcast(., County ~ Year, value.var = c("N", "Site", "point")) 

write_xlsx(list("Group" = sum.tab.group,
                "Single" = sum.tab.single),
                "Results/sum_table_1211.xlsx")


#### for summary table - survey point info

all.info.1 %>% setDT %>% 
  .[, list(Survey, TypeName,Year)] %>% table




