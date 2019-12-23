#---- load library

library(data.table)
library(magrittr)
library(readxl)
library(tidyr)
library(purrr)
library(dplyr)
library(writexl)
library(ggplot2)
library(lattice)

#---- 

M.data <- read_excel("./data/clean/for analysis.xlsx",
                     sheet=1) %>% setDT %>% 
  .[, DATE := as.IDate(paste(Year, Month, Day, sep = "/"))] %>% 
  .[TypeName %like% "混", TypeName.n := "混淆林"] %>% 
  .[TypeName %like% "竹林", TypeName.n := "竹林"] %>% 
  .[TypeName %like% "闊葉", TypeName.n := "闊葉林"] %>% 
  .[TypeName %like% "針葉", TypeName.n := "針葉林"] %>% 
  .[, TypeName.20 := ifelse(Distance>0, "非森林", TypeName.n)] %>% 
  
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
                     "台東縣","臺東縣"), Region := "East"] %>% 
  .[, julian.D := yday(DATE)] %>% 
  .[, Altitude_c := substr(Site_N,1,1)]


M.data %>% 
  .[Year < 2019,] %>% 
  .[!(TypeName.20 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>% 
  .[, .(V1 = sum(Macaca_sur),.N), by= list(Year, Altitude)] %>% 
  xyplot((V1/N) ~ Altitude, data = .,
         scales=list( cex=1.5),
       pch=16, col = "black",
       auto.key = list(space = "right"),
       ylab = "Encounter rate")

M.data %>% 
  .[Year < 2019,] %>% 
  .[!(TypeName.20 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>% 
  .[, Region := ordered(Region, c("East", "Center", "South", "North"))] %>% 
  .[, .(V1 = sum(Macaca_sur),.N), by= list(Year, Region)] %>% 
  bwplot((V1/N) ~ Region,  data = .,
         scales=list( cex=1.5),
         pch = "|",
         xlab = "Region",
         ylab = "Encounter rate")

M.data %>% 
  .[Year < 2019,] %>% 
  .[!(TypeName.20 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>% 
  .[, .(V1 = sum(Macaca_sur),.N), by= list(Year, julian.D,Altitude_c)] %>% 
  xyplot((V1/N) ~ julian.D, groups = Altitude_c,  data = .,
         #par.settings=list(superpose.symbol=list(pch=c(1,2,0),cex=0.9, col='black')),
         scales=list( cex=1.5),
         #auto.key = list(space = "right"),
         pch=16,
         xlab = "Julian day",
         ylab = "Encounter rate",
         xlim = c(50,190))

M.data %>% 
  .[Year < 2019,] %>% 
  .[!(TypeName.20 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>% 
  .[, TypeName.20 := ordered(TypeName.20, c("闊葉林", "針葉林", "混淆林", "竹林"))] %>% 
  .[, .(V1 = sum(Macaca_sur),.N), by= list(Year, TypeName.20)] %>% 
  bwplot((V1/N) ~ TypeName.20,  data = .,
         scales=list( cex=1.5),
          pch = "|",
          xlab = "TypeName",
          ylab = "Encounter rate")



M.data %>%
  .[Macaca_sur %in% 1,] %>% 
  .$Distance %>% hist(.,  plot=T,xlab = "Distance",
                      ylim = c(0,250),
                      main = "Group", col=gray(0.3))


