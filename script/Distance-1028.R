library(Distance)
library(data.table)
library(magrittr)
library(ggplot2)
library(readxl)

setwd("D:/R/test/Macaca-population-trend")


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



ttt<- S.all %>% setDT %>%  
  .[Macaca_dist %in% c("A","B"),] %>%
  .[Time %in% c("A","B"),] %>%
  .[Macaca_sur == 1,] %>% 
  .[Macaca_dist  %in% "A", distance := 25] %>%
  .[Macaca_dist  %in% "B", distance := 100] %>% setDF  
  
ds.ttt <- ds(ttt, region.table= , transect = "point", formula = ~ 1 ,
             dht.group =1, adjustment = NULL)

plot(ds.ttt,  breaks =c(0,25,100), pl.col =2)
summary(ds.ttt ) 
print(ds.ttt)


hist(ttt$distance,  breaks =c(0,25,100), probability  = T, plot=F)
table(ttt$distance)
