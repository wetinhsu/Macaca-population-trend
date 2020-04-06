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

M.data <- read_excel("./data/clean/for analysis_V1.xlsx",
                     sheet=1) %>% setDT %>%
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
                          "台東縣","臺東縣"))]  
  


M.data$Year %<>% as.numeric
M.data$Survey %<>% as.numeric
M.data$Point %<>% as.numeric
M.data$Macaca_sur %<>% as.numeric
M.data$Month %<>% as.numeric
M.data$Day %<>% as.numeric
M.data$Distance %<>% as.numeric


M.data %>%.[!(TypeName.1 %in% "非森林"),] %>%.[Macaca_sur %in%1,] %>% .[, .N]

(output.1<- M.data %>% 
  .[!(TypeName.1 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>%
  setDT %>%
  .[, .(N = .N, m = sum(Macaca_sur)), by = list(TypeName.1,TypeName)])

(output.2<- M.data %>% 
  .[!(TypeName.1 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>%
  setDT %>%
  
  .[, .(N = .N, m = sum(Macaca_sur)), by = list(Region2,County,Year,Survey)] %>%
  .[fread(("./data/clean/gis/county area-2.csv"), col.names =c("County", "Area", "perimeter")), on = "County"] %>% 
  .[, perimeter := NULL] %>%
  .[!(County %in% c("金門縣","澎湖縣", "連江縣")),] %>%
  .[, .(area = sum(Area), N= sum(N), m=sum(m)), by = list(Region2,Year, Survey )] %>% 
    .[, m:= as.numeric(m)] %>%  
    .[, N:= as.numeric(N)] %>% 
    melt(id.vars=c("Region2", "Year", "Survey","area"))%>% 
  dcast(Region2+ area + variable~ Year+Survey, value.var = c("value")) %>% 
    .[ order(Region2 ,variable),])



#-----------------------------------

(output.4<- M.data %>%  
  .[Macaca_sur %in% 1,] %>%
  .[,.(ll=length(Macaca_sur),
       mm=length(unique(Site_N)),
       kk=length(unique(paste0(Site_N,"-",Point)))), by = list(Year,  County)]%>%
  dcast(.,County ~ Year , value.var = c( "ll","mm","kk")))



(output.5<- M.data %>%   
  .[Macaca_sur %in% 0 & !is.na(Macaca_dist),] %>% 
  .[,.(ll=length(Macaca_sur),
       mm=length(unique(Site_N)),
       kk=length(unique(paste0(Site_N,"-",Point)))), by = list(Year,  County)]%>%
  dcast(.,County ~ Year , value.var = c( "ll","mm","kk")))


(output.4.1<- M.data %>%  
    .[Macaca_sur %in% 1,] %>%
    .[!(TypeName.1 %in% "非森林"),] %>% 
    .[,.(ll=length(Macaca_sur),
         mm=length(unique(Site_N)),
         kk=length(unique(paste0(Site_N,"-",Point)))), by = list(Year,  County)]%>%
    dcast(.,County ~ Year , value.var = c( "ll","mm","kk")))



(output.5.1<- M.data %>%   
    .[Macaca_sur %in% 0 & !is.na(Macaca_dist),] %>% 
    .[!(TypeName.1 %in% "非森林"),] %>% 
    .[,.(ll=length(Macaca_sur),
         mm=length(unique(Site_N)),
         kk=length(unique(paste0(Site_N,"-",Point)))), by = list(Year,  County)]%>%
    dcast(.,County ~ Year , value.var = c( "ll","mm","kk")))





(output.6<- M.data %>%    
  .[,.(N=.N, M = sum(Macaca_sur,na.rm=T)),
    by = list(Year, Survey, TypeName.1)]%>%
    .[, M:= as.numeric(M)] %>% 
    .[, N:= as.numeric(N)] %>% 
    melt(id.vars = c("Year" ,"Survey" ,"TypeName.1")) %>% 
    reshape2::dcast(., Year + Survey+ variable ~TypeName.1 , value.var = c("value")))


(output.7<- M.data %>% 
  #.[!(TypeName.1 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>% 
  .[Macaca_sur %in% 1, .N, list(TypeName.1, Macaca_dist)] %>% 
  dcast(., TypeName.1~Macaca_dist, value.var = "N"))

write_xlsx(list(
  TypeName_point = output.1,
  County_point= output.2,

  group_County = output.4,
  single_County = output.5,
  group_County_only_forest = output.4.1,
  single_County_only_forest = output.5.1,
  
  Forest_Macaca = output.6,
  Macaca_dist = output.7),
  paste0("./result/tables_",format(Sys.Date(),"%y%m%d"),".xlsx"))

#know your data------------------------------------------
M.data %>% .[, .N, by = list(Year, Survey)]
M.data %>% dcast( County ~ Year + Survey, value.var = "Point", length)
M.data %>% dcast( TypeName.1 + TypeName ~ Year + Survey, value.var = "Point", length) 
M.data %>% .[, Altitude_c := cut(Altitude, breaks=seq(0,4000,500), include.lowest = T)] %>%
  dcast( Altitude_c ~ Year + Survey, value.var = "Point", length) 
M.data %>%  dcast( julian.D ~ Year + Survey, value.var = "Point", length) %>% View()

  
  

#--------------------------
M.data %>% 
  .[!(TypeName.1 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>%
  .[, .(V1 = sum(Macaca_sur),.N), by= list(Year, Survey)] %>% 
  .[, Encounter_rate := V1/N] %>% 
  .[, .(V1 = sum(V1),
        N = sum(N),
        Encounter_rate = mean(Encounter_rate),
        Se = sd(Encounter_rate)/sqrt(length(Encounter_rate))), by= list(Year)] %>% 
  
  ggplot(., aes( Year, Encounter_rate)) +
  geom_point()+geom_line() +
  geom_errorbar(aes(ymin = (Encounter_rate - Se),
                    ymax = (Encounter_rate + Se)),
                 width = 0.1) +
  annotate("text",x=2015.5, y=0.03,label=paste0("mean ± se"), vjust=0,  color="red", size=5)+
  ylim(0,0.03)+
  theme_bw() + 
  xlab("Year")



Alt.d <- 
M.data %>% 
  .[!(TypeName.1 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>%
  .[, Altitude_f := cut(Altitude,
                        breaks = c(seq(0,4000,500)),
                        labels = c(seq(250,3750,500)),
                        include.lowest = T)] %>% 
  .[, .(V1 = sum(Macaca_sur),.N), by= list(Year, Survey, Altitude_f)] %>% 
  .[, Encounter_rate := V1/N] 


ggplot(Alt.d, aes( x=Altitude_f, y = Encounter_rate)) +
  geom_boxplot(width=0.8) +
  geom_smooth(data =Alt.d,aes(x=Altitude_f, y = Encounter_rate) ,method = "loess")+
  theme_bw() + xlab("Altitude")


#----

Alt.d <- 
  M.data %>% 
  .[!(TypeName.1 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>%
  .[, Altitude_f := cut(Altitude,
                        breaks = c(seq(0,4000,250)),
                        labels = c(seq(0,3750,250)),
                        include.lowest = T)] %>% 
  .[, .(V1 = sum(Macaca_sur),.N), by= list(Year, Survey, Altitude_f)] %>% 
  .[, Encounter_rate := V1/N] 


Alt.d3 <-
  Alt.d %>%  
  .[, .(Encounter_rate.m = mean(V1/N),
        sd =sd(V1/N),
        n = .N), by = list(Altitude_f)] %>% 
  .[, CI :=  sd/sqrt(n)]
  



ggplot(Alt.d3, aes( x= as.numeric(as.character(Altitude_f)), y = Encounter_rate.m)) +
  geom_bar( stat ="identity",width = 250,
            fill = gray(0.8), col = "black") + 
  geom_errorbar(aes(ymin = (Encounter_rate.m - CI),
                    ymax = (Encounter_rate.m + CI)),
                  position = "dodge", width = 100) +
  annotate("text",x=100, y=0.046,label=paste0("mean ± se"), vjust=0,  color="red", size=5)+
  theme_bw() + 
  scale_x_continuous("Altitude", breaks = seq(-125,3750,250),
                     labels = seq(0,3750,250))+
  scale_y_continuous("Encounter_rate")





#----
Rgn.d <- 
M.data %>% 
  .[!(TypeName.1 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>% 
  .[, Region2 := ordered(Region2, c("North","Center1", "Center2", "South","East1", "East2" ))] %>% 
  .[, .(V1 = sum(Macaca_sur),.N), by= list(Year, Survey, Region2)] %>% 
  .[, Encounter_rate := V1/N] 

ggplot(Rgn.d, aes( Region2, Encounter_rate))+
  geom_boxplot(width=0.4 )+
  theme_bw()+
  scale_x_discrete("Region", labels = c("North" = "北部",
                                         "Center1" = "中彰投",
                                         "Center2" = "雲嘉南",
                                         "South" = "高屏",
                                         "East1" = "花蓮",
                                         "East2" = "台東"))





Jd.d <- 
M.data %>% 
  .[!(TypeName.1 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>% 
  .[, julian.D_f := cut(julian.D,
                        breaks = c(seq(0,210,15)),
                        include.lowest = T)] %>% 
  .[, .(V1 = sum(Macaca_sur),.N), by= list(Year, Survey, julian.D_f)] %>% 
  .[, Encounter_rate := V1/N]   


ggplot(Jd.d, aes(julian.D_f, Encounter_rate))+
  geom_boxplot(width=0.4) +
  theme_bw() + scale_x_discrete("Julian day")+
  theme(axis.text.x = element_text(angle = 270, vjust = 0))




Type.d <- 
M.data %>% 
  .[!(TypeName.1 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>%
  .[, TypeName.1:= ordered(TypeName.1, c("闊葉林", "針葉林", "混淆林", "竹林"))] %>% 
  .[, .(V1 = sum(Macaca_sur),.N), by= list(Year, Survey, TypeName.1)]   %>% 
  .[, Encounter_rate := V1/N]     


  ggplot(Type.d, aes(TypeName.1, Encounter_rate))+
    geom_boxplot(width=0.4) +
    theme_bw() + 
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
