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
#library(reshape2)

#Original data---- 

M.data <- read_excel("./data/clean/for analysis.xlsx",
                     sheet=1) %>% setDT


M.data$Year %<>% as.numeric
M.data$Survey %<>% as.numeric
M.data$Point %<>% as.numeric
M.data$Macaca_sur %<>% as.numeric
M.data$Month %<>% as.numeric
M.data$Day %<>% as.numeric
M.data$Distance %<>% as.numeric


M.data %>%.[!(TypeName.1 %in% "非森林"),] %>%.[Macaca_sur %in%1,] %>% .[, .N]
M.data %>% 
  .[Year < 2019,] %>% 
  .[!(TypeName.1 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>%
  setDT %>%
  .[, .(N = .N, m = sum(Macaca_sur)), by = list(TypeName.1,TypeName)]

M.data %>% 
  .[Year < 2019,] %>% 
  .[!(TypeName.1 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>%
  setDT %>%
  .[, .(N = .N, m = sum(Macaca_sur)), by = list(Region2, County)]

M.data %>% 
  .[Year < 2019,] %>% 
  .[!(TypeName.1 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>%
  setDT %>%
  .[, .(N = .N, m = sum(Macaca_sur)), by = list(Region2)]

#-----------------------------------

M.data %>%  
  .[Macaca_sur %in%1,] %>%
  .[,.(ll=length(Macaca_sur),
       mm=length(unique(Site_N)),
       kk=length(unique(paste0(Site_N,"-",Point)))), by = list(Year,  County)]%>%
  dcast(.,County ~ Year , value.var = c( "ll","mm","kk"))



M.data %>%   
  .[Macaca_sur %in% 0 & !is.na(Macaca_dist),] %>% 
  .[,.(ll=length(Macaca_sur),
       mm=length(unique(Site_N)),
       kk=length(unique(paste0(Site_N,"-",Point)))), by = list(Year,  County)]%>%
  dcast(.,County ~ Year , value.var = c( "ll","mm","kk"))





M.data %>%    
  .[,.(ll=length(Macaca_sur), M = sum(Macaca_sur,na.rm=T)), by = list(Year, Survey, TypeName.1)]%>%
  dcast(.,Year + Survey ~ TypeName.1, value.var = c("ll", "M"))


M.data %>% 
  .[Year < 2019,] %>% 
  #.[!(TypeName.1 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>% 
  .[Macaca_sur %in% 1, .N, list(TypeName.1, Macaca_dist)] %>% 
  dcast(., TypeName.1~Macaca_dist, value.var = "N")

#------------------------------------------


M.data %>% 
  .[Year < 2019,] %>% 
  .[!(TypeName.1 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>%
  .[, .(V1 = sum(Macaca_sur),.N), by= list(Year, Survey)] %>% 
  .[, Encounter_rate := V1/N] %>% 
  .[, .(V1 = sum(V1),
        N = sum(N),
        Encounter_rate = mean(Encounter_rate)), by= list(Year)] %>% 
  
  ggplot(., aes( Year, Encounter_rate)) +
  geom_text(aes(label=V1), vjust=-2, color="red", size=3.5)+
  geom_text(aes(label=N), vjust=-0.5, color="black", size=3.5)+
  geom_text(aes(x=2015.5, y=0.024,label="猴群數"),  color="red", size=3.5)+
  geom_text(aes(x=2015.5, y=0.023,label="資料筆數"), color="black", size=3.5)+
  geom_point()+geom_line() +
  ylim(0.010,0.025)+
  theme_bw() + 
  xlab("Year")



Alt.d <- 
M.data %>% 
  .[Year < 2019,] %>% 
  .[!(TypeName.1 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>%
  .[, Altitude_f := cut(Altitude,
                        breaks = c(seq(0,4000,500)),
                        labels = c(seq(0,3500,500)),
                        include.lowest = T)] %>% 
  .[, .(V1 = sum(Macaca_sur),.N), by= list(Year, Survey, Altitude_f)] %>% 
  .[, Encounter_rate := V1/N]

Alt.d2 <-
  Alt.d %>% copy %>% 
  .[, .(N = sum(V1), V1 =sum(N)), by = list(Altitude_f)]


ggplot(Alt.d, aes( Altitude_f, Encounter_rate)) +
  geom_boxplot() +
  geom_text(data=Alt.d2, aes(x=Altitude_f, y = 0.040, label=V1), vjust=-2, color="red", size=3.5)+
  geom_text(data=Alt.d2, aes(x=Altitude_f, y = 0.045, label=N), vjust=-0.5, color="black", size=3.5)+
  #geom_text(aes(x=2015.5, y=0.024,label="猴群數"),  color="red", size=3.5)+
  #geom_text(aes(x=2015.5, y=0.023,label="資料筆數"), color="black", size=3.5)+
  theme_bw() + xlab("Altitude")

Rgn.d <- 
M.data %>% 
  .[Year < 2019,] %>% 
  .[!(TypeName.1 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>% 
  .[, Region2 := ordered(Region2, c("East1", "East2","Center1", "Center2", "South", "North"))] %>% 
  .[, .(V1 = sum(Macaca_sur),.N), by= list(Year, Survey, Region2)] %>% 
  .[, Encounter_rate := V1/N] 

Rgn.d2 <-
  Rgn.d %>% copy %>% 
  .[, .(N = sum(V1), V1 =sum(N)), by = list(Region2)]


ggplot(Rgn.d, aes( Region2, Encounter_rate))+geom_boxplot() +
  geom_text(data=Rgn.d2, aes(x=Region2, y = 0.085, label=V1), vjust=-2, color="red", size=3.5)+
  geom_text(data=Rgn.d2, aes(x=Region2, y = 0.095, label=N), vjust=-0.5, color="black", size=3.5)+
  #geom_text(aes(x=2015.5, y=0.024,label="猴群數"),  color="red", size=3.5)+
  #geom_text(aes(x=2015.5, y=0.023,label="資料筆數"), color="black", size=3.5)+
  theme_bw()

Jd.d <- 
M.data %>% 
  .[Year < 2019,] %>% 
  .[!(TypeName.1 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>% 
  .[, julian.D_f := cut(julian.D,
                        breaks = c(seq(0,210,15)),
                        include.lowest = T)] %>% 
  .[, .(V1 = sum(Macaca_sur),.N), by= list(Year, Survey, julian.D_f)] %>% 
  .[, Encounter_rate := V1/N]   

Jd.d2 <-
  Jd.d %>% copy %>% 
  .[, .(N = sum(V1), V1 =sum(N)), by = list(julian.D_f)]

ggplot(Jd.d, aes(julian.D_f, Encounter_rate))+
  geom_boxplot() +
  geom_text(data=Jd.d2, aes(x=julian.D_f, y = 0.040, label=V1), vjust=-2, color="red", size=3.5)+
  geom_text(data=Jd.d2, aes(x=julian.D_f, y = 0.047, label=N), vjust=-0.5, color="black", size=3.5)+
  #geom_text(aes(x=2015.5, y=0.024,label="猴群數"),  color="red", size=3.5)+
  #geom_text(aes(x=2015.5, y=0.023,label="資料筆數"), color="black", size=3.5)+
  theme_bw() + 
  xlab("Julian day")




Type.d <- 
M.data %>% 
  .[Year < 2019,] %>% 
  .[!(TypeName.1 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>%
  .[, TypeName.1:= ordered(TypeName.1, c("闊葉林", "針葉林", "混淆林", "竹林"))] %>% 
  .[, .(V1 = sum(Macaca_sur),.N), by= list(Year, Survey, TypeName.1)]   %>% 
  .[, Encounter_rate := V1/N]     

Type.d2 <-
  Type.d %>% copy %>% 
  .[, .(N = sum(V1), V1 =sum(N)), by = list(TypeName.1)] 

  
  ggplot(Type.d, aes(TypeName.1, Encounter_rate))+
    geom_boxplot() +
    geom_text(data=Type.d2, aes(x=TypeName.1, y = 0.040, label=V1), vjust=-2, color="red", size=3.5)+
    geom_text(data=Type.d2, aes(x=TypeName.1, y = 0.045, label=N), vjust=-0.5, color="black", size=3.5)+
    #geom_text(aes(x=2015.5, y=0.024,label="猴群數"),  color="red", size=3.5)+
    #geom_text(aes(x=2015.5, y=0.023,label="資料筆數"), color="black", size=3.5)+
    theme_bw() + 
    xlab("Forest type")
  



M.data %>%setDT %>% 
  .[Year < 2019,] %>% 
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

  
M.data %>% 
  .[Year < 2019,] %>% 
  .[!(TypeName.1 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>%
  .[, .(V1 = sum(Macaca_sur),.N), by= list(Year, Survey, Region2)] %>% 
  .[, Encounter_rate := V1/N] %>% 
  .[, .(V1 = sum(V1),
        N = sum(N),
        Encounter_rate = mean(Encounter_rate)), by= list(Year, Region2)] %>% 
  
  ggplot(., aes( Year, Encounter_rate, group = Region2, col= Region2))+
  geom_point() +
  geom_line()+ 
  theme_bw()+ 
  xlab("Year")


M.data %>% 
  .[Year < 2019,] %>% 
  .[!(TypeName.1 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>%
  .[, .(V1 = sum(Macaca_sur),.N), by= list(Year, Survey, TypeName.1)] %>% 
  .[, Encounter_rate := V1/N] %>% 
  .[, .(V1 = sum(V1),
        N = sum(N),
        Encounter_rate = mean(Encounter_rate)), by= list(Year,TypeName.1)] %>% 
  
  ggplot(., aes( Year, Encounter_rate, group = TypeName.1,col= TypeName.1))+
  geom_point() +
  geom_line()+ 
  theme_bw()+ 
  xlab("Year")


