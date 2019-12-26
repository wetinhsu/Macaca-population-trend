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
library(reshape2)

#Original data---- 

M.data.o <- read_excel("./data/clean/for analysis.xlsx",
                     sheet=1) %>% setDT %>% 
  .[, DATE := as.IDate(paste(Year, Month, Day, sep = "/"))] %>% 
  .[TypeName %like% "混", TypeName.n := "混淆林"] %>% 
  .[TypeName %like% "竹林", TypeName.n := "竹林"] %>% 
  .[TypeName %like% "闊葉", TypeName.n := "闊葉林"] %>% 
  .[TypeName %like% "針葉", TypeName.n := "針葉林"] %>% 
  .[, TypeName.1 := ifelse(Distance>20, "非森林", TypeName.n)] %>% 
  .[, TypeName.1 := ordered(TypeName.1,c("闊葉林", "針葉林","混淆林","竹林","非森林"))] %>% 
  .[, County := ordered(County,
                        c("宜蘭縣","基隆市","台北市","臺北市",
                          "新北市","台北縣","臺北縣",
                          "桃園縣","桃園市","新竹市",
                          "新竹縣","苗栗縣",
                          "台中市","臺中市","台中縣","臺中縣",
                          "彰化縣","南投縣","南投市",
                          "雲林縣","嘉義縣","嘉義市",
                          "台南市","臺南市","台南縣","臺南縣",
                          "高雄縣","高雄市",
                          "屏東縣", "花蓮縣",
                          "台東縣","臺東縣"))] %>% 
  
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
  .[, Altitude_c := substr(Site_N,1,1)] %>% setDT 

M.data.o$Year %<>% as.numeric
M.data.o$Survey %<>% as.numeric
M.data.o$Point %<>% as.numeric
M.data.o$Macaca_sur %<>% as.numeric
M.data.o$Month %<>% as.numeric
M.data.o$Day %<>% as.numeric
M.data.o$Distance %<>% as.numeric

#Remove duplicate data-------------------------------------------
M.data <- M.data.o %>% copy(.) %>% 
  .[Year %in% 2015 & Survey %in% 2 & Site_N %in% "A29-17" & Point %in% 7,
    Macaca_sur := NA] %>% 
  .[Year %in% 2015 & Survey %in% 2 & Site_N %in% "A33-28" & Point %in% 7,
    Macaca_sur := NA] %>% 
  .[Year %in% 2016 & Survey %in% 1 & Site_N %in% "B33-01" & Point %in% 4,
    Macaca_sur := NA] %>% 
  .[Year %in% 2017 & Survey %in% 1 & Site_N %in% "B38-07" & Point %in% 7,
    Macaca_sur := NA] %>% 
  .[Year %in% 2018 & Survey %in% 1 & Site_N %in% "A35-15" & Point %in% 5,
    Macaca_sur := NA] %>% 
  .[Year %in% 2018 & Survey %in% 2 & Site_N %in% "A28-10" & Point %in% 6,
    Macaca_sur := NA] %>% 
  .[Year %in% 2019 & Survey %in% 1 & Site_N %in% "B14-02" & Point %in% 6,
    Macaca_sur := NA] %>% 
  .[Year %in% 2019 & Survey %in% 1 & Site_N %in% "B38-08" & Point %in% 5,
    Macaca_sur := NA] %>% 
  .[Year %in% 2019 & Survey %in% 2 & Site_N %in% "A20-02" & Point %in% 3,
    Macaca_sur := NA] %>% 
  .[Year %in% 2019 & Survey %in% 2 & Site_N %in% "A33-32" & Point %in% 6,
    Macaca_sur := NA] 

M.data.o %>% .[Macaca_sur %in% 1, .N]
M.data %>% .[Macaca_sur %in% 1, .N]
#-----------------------------------

M.data.o %>% .[Macaca_sur %in% 1, .N, by = list(Year, Survey, County)] %>% 
  dcast(., County ~ Year + Survey, value.var = "N",
        fun.aggregate = sum,
        margins = c("Year", "County"))

M.data %>% .[Macaca_sur %in% 1, .N, by = list(Year, Survey, County)] %>% 
  dcast(., County ~ Year + Survey, value.var = "N",
        fun.aggregate = sum,
        margins = c("Year", "County"))



M.data %>% .[Macaca_sur %in% 1, .N, by = list(Year, Survey, TypeName.1)] %>% 
  dcast(., Year + Survey ~ TypeName.1, value.var = "N",
        fun.aggregate = sum,
        margins = c("Year", "County"))



#------------------------------------------
M.data %>% 
  .[Year < 2019,] %>% 
  .[!(TypeName.1 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>%
  .[, .(V1 = sum(Macaca_sur),.N), by= list(Year)] %>% 
  .[, Encounter_rate := V1/N] %>% 
  
  ggplot(., aes( Year, Encounter_rate)) +
  geom_bar(stat="identity", colour = "black", fill=gray(0.8)) +
  theme_bw() + 
  xlab("Year")
  

M.data %>% 
  .[Year < 2019,] %>% 
  #.[!(TypeName.1 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>% 
  .[Macaca_sur %in% 1, .N, list(TypeName.1, Macaca_dist)] %>% 
  dcast(., TypeName.1~Macaca_dist, value.var = "N")


M.data %>% 
  .[Year < 2019,] %>% 
  .[!(TypeName.1 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>% 
  .[, Altitude_f := cut(Altitude,
                        breaks = c(seq(0,4000,500)),
                        labels = c(seq(0,3500,500)),
                        include.lowest = T)] %>% 
  .[, .(V1 = sum(Macaca_sur),.N), by= list(Year, Altitude_f)] %>% 
  .[, Encounter_rate := V1/N] %>% 

ggplot(., aes( Altitude_f, Encounter_rate)) + geom_boxplot() + theme_bw() + 
  xlab("Altitude")


M.data %>% 
  .[Year < 2019,] %>% 
  .[!(TypeName.1 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>% 
  .[, Region := ordered(Region, c("East", "Center", "South", "North"))] %>% 
  .[, .(V1 = sum(Macaca_sur),.N), by= list(Year, Region)] %>% 
  .[, Encounter_rate := V1/N] %>% 

ggplot(., aes( Region, Encounter_rate))+geom_boxplot() + theme_bw()

M.data %>% 
  .[Year < 2019,] %>% 
  .[!(TypeName.1 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>% 
  .[, .(V1 = sum(Macaca_sur),.N), by= list(Year, julian.D, Altitude_c )] %>% 
  .[, Encounter_rate := V1/N] %>% 

ggplot(., aes(julian.D, Encounter_rate, colour = Altitude_c))+
  geom_point() + theme_bw()


M.data %>% 
  .[Year < 2019,] %>% 
  .[!(TypeName.1 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>%
  .[, julian.D_f := cut(julian.D,
                        breaks = c(seq(0,210,15)),
                        include.lowest = T)] %>% 
  .[, .(V1 = sum(Macaca_sur),.N), by= list(Year, julian.D_f)] %>% 
  .[, Encounter_rate := V1/N]  %>% 

ggplot(., aes(julian.D_f, Encounter_rate))+
  geom_boxplot() + theme_bw() + 
  xlab("Julian day")





M.data %>% 
  .[Year < 2019,] %>% 
  .[!(TypeName.1 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>% 
  .[, TypeName.1:= ordered(TypeName.1, c("闊葉林", "針葉林", "混淆林", "竹林"))] %>% 
  .[, .(V1 = sum(Macaca_sur),.N), by= list(Year, TypeName.1)]  %>% 
  .[, Encounter_rate := V1/N] %>% 
  
  ggplot(., aes( TypeName.1, Encounter_rate))+geom_boxplot() + theme_bw()+ 
  xlab("Forest type")



M.data %>%setDT %>% 
  .[Macaca_sur %in% 1,DD := cut(Distance, breaks = seq(0,100,10),
                                ordered_result = T,
                                include.lowest = T)] %>% 
  .[Macaca_sur %in% 1,.(D2 = .N), by = list(DD)] %>% 
  ggplot(., aes(x=DD, y=D2)) +
  geom_bar(stat="identity", colour = "black", fill=gray(0.8))+
  geom_text(aes(label=D2), vjust=-0.3, color="black", size=3.5)+
  theme_bw()+
  xlab("Distance")+
  ylab("Count")

  

