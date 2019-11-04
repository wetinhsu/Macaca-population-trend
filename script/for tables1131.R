# for summary table

library(readxl)
library(data.table)
library(magrittr)
library(dplyr)
library(writexl)

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
  .[ !is.na(Do.survey),]


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



sum.table.0 <- S.all %>%     #total survey point
  .[ Do.survey %in% 1, ] %>%
  .[, .(data_N = length(unique(paste0(Site_N,Point)))), by = list(Year, Survey)] %>%
  dcast(., Year ~ Survey , value.var = c("data_N"))

sum.table.0.1 <- S.all %>%     #total survey point <100m for group
  .[ Do.survey %in% 1, ] %>%
  .[ Macaca_dist %in% c("A","B"), ] %>%
  .[ Macaca_sur %in% 1, ] %>%
  .[, .(data_N = length(unique(paste0(Site_N,Point)))), by = list(Year, Survey)] %>%
  dcast(., Year ~ Survey , value.var = c("data_N"))



sum.table.1<- S.all %>%     #total survey point ~ county  for group
  .[Macaca_sur %in% 1, ] %>%
  .[, .(data_N = length(unique(paste0(Site_N,Point,Survey))),
        site_N = length(unique(Site_N)),
        point_N = length(unique(paste0(Site_N,Point)))), by = list(Year, County)] %>%
  dcast(., County ~ Year, value.var = c("data_N", "site_N","point_N"))

sum.table.2<- S.all %>%       #total survey point ~ county for single 
  .[Macaca_sur %in% 0, ] %>%
  .[, .(data_N = length(unique(paste0(Site_N,Point,Survey))),
        site_N = length(unique(Site_N)),
        point_N = length(unique(paste0(Site_N,Point)))), by = list(Year, County)] %>%
  dcast(., County ~ Year, value.var = c("data_N", "site_N","point_N"))

sum.table.3<- S.all %>%    #survey point ~ TypeName  for all survey 
  .[ Do.survey %in% 1, ] %>%
  .[ Distance <20 , ] %>%
  .[ ! is.na(TypeName.1) , ] %>%
  .[, .(data_N = length(unique(paste0(Site_N,Point)))), by = list(Year, Survey, TypeName.1)] %>%
  dcast(., Year + Survey ~ TypeName.1, value.var = c("data_N"))

sum.table.4<- S.all %>%    #survey point ~ TypeName  for group
  .[ Do.survey %in% 1, ] %>%
  .[ Distance <20 , ] %>%
  .[ Macaca_sur %in% 1, ] %>%
  .[ ! is.na(TypeName.1) , ] %>%
  .[, .(data_N = length(unique(paste0(Site_N,Point)))), by = list(Year, Survey, TypeName.1)] %>%
  dcast(., Year + Survey ~ TypeName.1, value.var = c("data_N"))


sum.table.5<- S.all %>%    #survey point ~ TypeName + Altitude for all survey 
  .[ Do.survey %in% 1, ] %>%
  .[ Distance <20 , ] %>%
  .[ ! is.na(TypeName.1) , ] %>%
  .[, .(data_N = length(unique(paste0(Site_N,Point)))), by = list(Year, Survey, TypeName.1, Altitude)] %>%
  dcast(., Year + Survey ~  Altitude +TypeName.1, value.var = c("data_N"))


sum.table.6<- S.all %>%    #survey point ~ TypeName + Altitude for group 
  .[ Do.survey %in% 1, ] %>%
  .[ Distance <20 , ] %>%
  .[ Macaca_sur %in% 1, ] %>%
  .[ ! is.na(TypeName.1) , ] %>%
  .[, .(data_N = length(unique(paste0(Site_N,Point)))), by = list(Year, Survey, TypeName.1, Altitude)] %>%
  dcast(., Year + Survey ~  Altitude +TypeName.1, value.var = c("data_N"))




write_xlsx(list(  # 除了sum.table.0.1 之外，其餘的表格都排除沒距離沒時段，距離在A、B、C之內。
  "total.survey.point" = sum.table.0 ,
  "point.100m.group" = sum.table.0.1,
  "county.group" = sum.table.1, 
  "county.single " = sum.table.2,
  "TypeName.Allsurvey" = sum.table.3,
  "TypeName.group" = sum.table.4, 
  "TypeNameAltitude.Allsurvey" = sum.table.5,
  "TypeNameAltitude.group" = sum.table.6 
),  "Results/sum_table_1031.xlsx")
