library(Distance)
library(data.table)
library(magrittr)
library(ggplot2)
library(readxl)

setwd("D:/R/test/Macaca-population-trend")

M.data <- 
  read_xlsx("data/clean/data_for_analysis.xlsx") %>% 
  setDT %>% 
  .[, Year := as.numeric(Year)] %>% 
  .[, Year.re := Year - min(Year) + 1] %>%
  .[, high := substr(Site_N, 1, 1)] %>%
  .[County %in% list("基隆市","台北市","臺北市",
                     "新北市","台北縣","臺北縣",
                     "桃園縣","桃園市","新竹市",
                     "新竹縣","苗栗縣"), Region := "North"] %>%
  .[County %in% list("台中市","臺中市",
                     "台中縣","臺中縣",
                     "彰化縣","南投縣","南投市",
                     "雲林縣","嘉義縣","嘉義市"), Region := "Center"] %>%
  .[County %in% list("台南市","臺南市",
                     "台南縣","臺南縣",
                     "高雄縣","高雄市",
                     "屏東縣"), Region := "South"]%>%
  .[County %in% list("宜蘭縣","花蓮縣",
                     "台東縣","臺東縣"), Region := "East"] 



ttt<- M.data %>% setDT %>%  
  #.[!(Macaca_dist %in% "C"),] %>%
  .[Macaca_sur == 1,] %>% 
  .[Macaca_dist  %in% "A", distance := 25] %>%
  .[Macaca_dist  %in% "B", distance := 100] %>%
  .[Macaca_dist  %in% "C", distance := 300] %>% setDF  
  
ds.ttt <- ds(ttt, region.table= , transect = "point", formula = ~ 1 ,
             dht.group =1, adjustment = NULL)

plot(ds.ttt,  breaks =c(0,25,100,300), pl.col =2)
summary(ds.ttt ) 

hist(ttt$distance,  breaks =c(0,25,100,300), probability  = T)
table(ttt$distance)
