library(Distance)
library(data.table)
library(magrittr)
library(ggplot2)
library(readxl)

setwd("D:/R/test/Macaca-population-trend")


M.data <- 
  read_xlsx("data/clean/data_for_analysis_1519.xlsx") %>% 
  setDT %>% 
  .[ Distance <20 , ] %>%
  .[, Year := as.numeric(Year)] %>% 
  .[, Year.re := Year - min(Year) + 1] %>%
  .[County %in% list("宜蘭縣","基隆市","台北市","臺北市",
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
  .[County %in% list("花蓮縣",
                     "台東縣","臺東縣"), Region := "East"] 



ttt<- M.data %>% setDT %>%  
  .[Macaca_dist %in% c("A","B"),] %>%
  .[Time %in% c("A","B"),] %>%
  .[Macaca_sur == 1,] %>% 
  .[Macaca_dist  %in% "A", distance := 25] %>%
  .[Macaca_dist  %in% "B", distance := 100] %>% setDF  
  
ds.ttt <- ds(ttt, transect = "point",
             formula = ~ 1 ,
             adjustment = NULL)

plot(ds.ttt,  breaks =c(0,25,100), pl.col =2)
summary(ds.ttt ) 
print(ds.ttt)
gof_ds(ds.ttt)

hist(ttt$distance,  breaks =c(0,25,100), probability  = T, plot=T)
table(ttt$distance)



data(amakihi)

ds.ttt <- ds(ttt, transect = "point",
             formula = ~ TypeName ,key = "hn",
             adjustment = NULL)

plot(ds.ttt,  breaks =c(0,25,100), pl.col =2)
summary(ds.ttt ) 
print(ds.ttt)
gof_ds(ds.ttt)
