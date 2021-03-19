#---- load library

library(data.table)
library(magrittr)
library(readxl)
library(tidyr)
library(purrr)
library(dplyr)
library(writexl)
library(ggplot2)


#Original data---- 

M.data <- read_excel("./data/clean/for analysis_V1.xlsx",
                     sheet=1) %>% setDT %>%
  #  .[analysis %in% "Y",] %>% 
  .[, TypeName.1 := ordered(TypeName.1,c("闊葉林",
                                         "針葉林",
                                         "混淆林",
                                         "竹林",
                                         "非森林"))] %>% 
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
                          "台東縣","臺東縣"))]  %>% 
  .[, Year := as.numeric(Year)] %>% 
  .[, Survey := as.numeric(Survey)] %>% 
  .[, Point := as.numeric(Point)] %>% 
  .[, Macaca_sur := as.numeric(Macaca_sur)] %>% 
  .[, Month := as.numeric(Month)] %>% 
  .[, Day := as.numeric(Day)] %>% 
  .[, Distance := as.numeric(Distance)]



M.data %>% .[analysis %in% "Y",] %>% 
  .[!(TypeName.1 %in% "非森林"),] %>%.[Macaca_sur %in%1,] %>% .[, .N]

#-----------------------------------

(output.1<- M.data %>% 
   .[analysis %in% "Y",] %>% 
   .[!(TypeName.1 %in% "非森林"),] %>% 
   .[is.na(Macaca_sur), Macaca_sur := 0] %>%
   .[, .(N = .N, m = sum(Macaca_sur)), by = list(TypeName.1,TypeName)])






(output.2<- 
    M.data %>% setDT %>%  
    .[analysis %in% "Y",] %>% 
    .[!(TypeName.1 %in% "非森林"),] %>%
    .[,.(N=.N, 
         m = sum(Macaca_sur,na.rm=T),
         E = sum(Macaca_sur,na.rm=T)/.N),
      by = list(Year, Survey, Region2)]%>%
    .[, m:= as.numeric(m)] %>% 
    .[, N:= as.numeric(N)] %>% 
    .[, E:= as.numeric(E)] %>% 
    melt(id.vars = c("Year" ,"Survey" ,"Region2")) %>% 
    dcast(., Region2+ variable ~ Year + Survey, value.var = c("value")) 
  
)



(output.4<- M.data %>%  
    .[analysis %in% "Y",] %>% 
    .[Macaca_sur %in% 1,] %>%
    .[,.(ll=length(Macaca_sur),
         mm=length(unique(Site_N)),
         kk=length(unique(paste0(Site_N,"-",Point)))), by = list(Year,  County)]%>%
    dcast(.,County ~ Year , value.var = c( "ll","mm","kk")))



(output.5<- M.data %>%   
    .[analysis %in% "Y",] %>% 
    .[Macaca_sur %in% 0 & !is.na(Macaca_dist),] %>% 
    .[,.(ll=length(Macaca_sur),
         mm=length(unique(Site_N)),
         kk=length(unique(paste0(Site_N,"-",Point)))), by = list(Year,  County)]%>%
    dcast(.,County ~ Year , value.var = c( "ll","mm","kk")))


(output.4.1<- M.data %>%  
    .[analysis %in% "Y",] %>% 
    .[Macaca_sur %in% 1,] %>%
    .[!(TypeName.1 %in% "非森林"),] %>% 
    .[,.(ll=length(Macaca_sur),
         mm=length(unique(Site_N)),
         kk=length(unique(paste0(Site_N,"-",Point)))), by = list(Year,  County)]%>%
    dcast(.,County ~ Year , value.var = c( "ll","mm","kk")))



(output.5.1<- M.data %>%   
    .[analysis %in% "Y",] %>% 
    .[Macaca_sur %in% 0 & !is.na(Macaca_dist),] %>% 
    .[!(TypeName.1 %in% "非森林"),] %>% 
    .[,.(ll=length(Macaca_sur),
         mm=length(unique(Site_N)),
         kk=length(unique(paste0(Site_N,"-",Point)))), by = list(Year,  County)]%>%
    dcast(.,County ~ Year , value.var = c( "ll","mm","kk")))





(output.6<- M.data %>%    
    .[TypeName.1 %in% "非森林", analysis := "Z"] %>% 
    .[,.(N=.N,
         m = sum(Macaca_sur,na.rm=T),
         E = sum(Macaca_sur,na.rm=T)/.N),
      by = list(Year, Survey, TypeName.1, analysis)]%>%
    .[, m:= as.numeric(m)] %>% 
    .[, N:= as.numeric(N)] %>% 
    .[, E:= as.numeric(E)] %>% 
    melt(id.vars = c("Year" ,"Survey" ,"TypeName.1", "analysis")) %>% 
    dcast(., analysis+TypeName.1+ variable ~ Year + Survey, value.var = c("value")) 
)


(output.7<- M.data %>% 
    #.[!(TypeName.1 %in% "非森林"),] %>% 
    .[is.na(Macaca_sur), Macaca_sur := 0] %>% 
    .[Macaca_sur %in% 1, .N, list(TypeName.1, Macaca_dist)] %>% 
    dcast(., TypeName.1~Macaca_dist, value.var = "N"))

(output.8<-M.data %>% 
    .[analysis %in% "Y",] %>% 
    .[!(TypeName.1 %in% "非森林"),] %>% 
    .[is.na(Macaca_sur), Macaca_sur := 0] %>%
    .[, .(m = sum(Macaca_sur),.N), by= list(Year, Survey)] %>% 
    .[, Encounter_rate := m/N] %>% 
    .[, .(Sum.m = sum(m),
          Mean.m = mean(m),
          Se.m = sd(m)/sqrt(length(m)),
          
          Sum.N = sum(N),
          Mean.N = mean(N),
          Se.N = sd(N)/sqrt(length(N)),
          
          Encounter_rate = mean(Encounter_rate),
          Se = sd(Encounter_rate)/sqrt(length(Encounter_rate))), by= list(Year)]
)

output.9<-
  M.data %>% setDT %>%   
  .[analysis %in% "Y",] %>% 
  .[!(TypeName.1 %in% "非森林"),] %>%
  .[,.(N=.N,
       m = sum(Macaca_sur,na.rm=T),
       E = sum(Macaca_sur,na.rm=T)/.N),
    by = list(Year, Survey, County)]%>%
  .[, m:= as.numeric(m)] %>% 
  .[, N:= as.numeric(N)] %>% 
  .[, E:= as.numeric(E)] %>% 
  melt(id.vars = c("Year" ,"Survey" ,"County")) %>% 
  dcast(., County+ variable ~ Year + Survey, value.var = c("value")) 

write_xlsx(list(
  TypeName_point = output.1,
  Year_point= output.8,
  Region_point= output.2,
  County_point= output.9,
  
  group_County = output.4,
  single_County = output.5,
  group_County_only_forest = output.4.1,
  single_County_only_forest = output.5.1,
  
  Forest_Macaca = output.6,
  Macaca_dist = output.7
  
),
paste0("./result/tables_2019",format(Sys.Date(),"%y%m%d"),".xlsx"))

#know your data------------------------------------------
M.data %>% .[, .N, by = list(Year, Survey)]
M.data %>% dcast( County ~ Year + Survey, value.var = "Point", length)
M.data %>% dcast( TypeName.1 + TypeName ~ Year + Survey, value.var = "Point", length) 
M.data %>% .[, Altitude_c := cut(Altitude, breaks=seq(0,4000,500), include.lowest = T)] %>%
  dcast( Altitude_c ~ Year + Survey, value.var = "Point", length) 
M.data %>%  dcast( julian.D ~ Year + Survey, value.var = "Point", length) %>% View()




#------------------  
M.data %>%
  filter(analysis %in% "Y") %>% 
  filter(!TypeName.1 %in% "非森林") %>% 
  mutate(Altitude_f = cut(Altitude,
                          breaks = c(seq(0,4000,500)),
                          labels = c(seq(250,3750,500)),
                          include.lowest = T)) %>% 
  group_by(Altitude_f) %>% 
  summarise(N = n())

#--------------------------

M.data %>% 
  .[analysis %in% "Y",] %>% 
  .[!(TypeName.1 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>%
  .[, .(V1 = sum(Macaca_sur),.N), by= list(Year, Survey)] %>% 
  .[, Encounter_rate := V1/N] %>% 
  .[, .(V1 = sum(V1),
        N = sum(N),
        Encounter_rate = mean(Encounter_rate),
        Se = sd(Encounter_rate)/sqrt(length(Encounter_rate))), by= list(Year)] %>% 
  
  ggplot(. , aes( Year, Encounter_rate)) +
  geom_point(size = 4)+
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = (Encounter_rate - Se),
                    ymax = (Encounter_rate + Se)),
                width = 0.1,size = 1) +
  #  annotate("text",x=2015.5, y=0.03,label=paste0("mean ± se"),
  #           vjust=0,  color="red", size=8,family="serif")+
  theme_classic() + 
  scale_y_continuous(limits = c(0,0.03),expand = c(0, 0.002, 0, 0))+
  labs(x = "Year", y = "Encounter rate (troop/point)" ) +
  theme(
    text = element_text(family="serif"),
    aspect.ratio = 1,
   # panel.border = element_rect(size = 1.5,fill = NA),
    axis.line = element_line(size = 1, colour = "black"),
    axis.ticks = element_line(size = 1),
    axis.text = element_text(size = 18,colour = "black"),
    axis.title = element_text(size = 20,colour = "black",
                              vjust = -2, hjust = 0.5),
    axis.title.x.bottom = element_text(vjust = -2),
    axis.title.y.left = element_text(vjust = 2),
    axis.ticks.length = unit(.25, "cm"),
    panel.grid = element_blank(),
    plot.margin = margin(30,30,20,20)
  )



Alt.d <- 
  M.data %>% 
  .[analysis %in% "Y",] %>% 
  .[!(TypeName.1 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>%
  .[, Altitude_f := cut(Altitude,
                        breaks = c(seq(0,4000,500)),
                        labels = c(seq(250,3750,500)),
                        include.lowest = T)] %>% 
  .[, .(V1 = sum(Macaca_sur),.N), by= list(Year, Survey, Altitude_f)] %>% 
  .[, Encounter_rate := V1/N] 



Alt.d.n <-
  Alt.d %>% 
  .[,.(mean_N = sum(N)/10, y = quantile(Encounter_rate,0.75)), by = list(Altitude_f)]

ggplot(data = Alt.d, aes( x=Altitude_f, y = Encounter_rate)) +
  geom_boxplot(size = 1, width = 0.4, fill= gray(.9),
               outlier.size = 3) +
  geom_text(data = Alt.d.n, aes(y = y+0.0018,label = paste0(mean_N,c("","","","","","","",""))),
            size = 3,
            hjust = -0.1,
            position = position_dodge(0.9))+
  
  labs(x = "Elevation (m)", y = "Encounter rate (troop/point)") +
  theme_classic() +
  scale_y_continuous(limits = c(0,0.08),expand = c(0, 0.002, 0, 0))+
  theme(
    text = element_text(family="serif"),
    aspect.ratio = 1,
    panel.border = element_rect(size = 1.5,fill = NA),
    axis.line = element_line(size = 1, colour = "black"),
    axis.ticks = element_line(size = 1),
    axis.text = element_text(size = 14,colour = "black"),
    axis.title = element_text(size = 18,colour = "black",
                              vjust = -2, hjust = 0.5),
    axis.title.x.bottom = element_text(vjust = -2),
    axis.title.y.left = element_text(vjust = 2),
    axis.ticks.length = unit(.25, "cm"),
    panel.grid = element_blank(),
    plot.margin = margin(30,30,20,20)
  )
ggsave("圖1.png",
       path = "./result_2019",
       width = 15,
       height = 15,
       units = "cm")

boxplot(Encounter_rate~Altitude_f, Alt.d,plot=F) #確認中位數
#only 闊葉林


Alt.d.B <- 
  M.data %>% 
  .[analysis %in% "Y",] %>% 
  #.[!(TypeName.1 %in% "非森林"),] %>% 
  .[TypeName.1 %in% "闊葉林",] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>%
  .[, Altitude_f := cut(Altitude,
                        breaks = c(seq(0,4000,500)),
                        labels = c(seq(250,3750,500)),
                        include.lowest = T)] %>% 
  .[, .(V1 = sum(Macaca_sur),.N), by= list(Year, Survey, Altitude_f)] %>% 
  #  .[, Altitude_f:= ordered(Altitude_f)] %>% 
  .[, Encounter_rate := V1/N] 


ggplot(Alt.d.B, aes( x=Altitude_f, y = Encounter_rate)) +
  geom_boxplot(size = 1, width = 0.4, fill= gray(.9),
               outlier.size = 3) +
  
  labs(x = "Altitude") +
  theme_bw() +
  theme(
    text = element_text(family="serif"),
    aspect.ratio = 1,
    panel.border = element_rect(size = 1.5,fill = NA),
    axis.line = element_line(size = 1, colour = "black"),
    axis.ticks = element_line(size = 1),
    axis.text = element_text(size = 18,colour = "black"),
    axis.title = element_text(size = 23,colour = "black",
                              vjust = -2, hjust = 0.5),
    axis.title.x.bottom = element_text(vjust = -2),
    axis.title.y.left = element_text(vjust = 2),
    axis.ticks.length = unit(.25, "cm"),
    panel.grid = element_blank(),
    plot.margin = margin(30,30,20,20)
  )




Rgn.d <- 
  M.data %>% 
  .[analysis %in% "Y",] %>% 
  .[!(TypeName.1 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>% 
  .[, Region2 := ordered(Region2, c("North","Center1", "Center2", "South","East1", "East2" ))] %>% 
  .[, .(V1 = sum(Macaca_sur),.N), by= list(Year, Survey, Region2)] %>% 
  .[, Encounter_rate := V1/N] 

ggplot(Rgn.d, aes( Region2, Encounter_rate))+
  geom_boxplot(size = 1, width = 0.4, fill= gray(.9),
               outlier.size = 3)+
  theme_bw()+
  scale_x_discrete("Region", labels = c("North" = "北部",
                                        "Center1" = "中彰投",
                                        "Center2" = "雲嘉南",
                                        "South" = "高屏",
                                        "East1" = "花蓮",
                                        "East2" = "台東"))+
  theme(
    text = element_text(family="serif"),
    aspect.ratio = 1,
    panel.border = element_rect(size = 1.5,fill = NA),
    axis.line = element_line(size = 1, colour = "black"),
    axis.ticks = element_line(size = 1),
    axis.text = element_text(size = 18,colour = "black"),
    axis.title = element_text(size = 23,colour = "black",
                              vjust = -2, hjust = 0.5),
    axis.title.x.bottom = element_text(vjust = -2),
    axis.title.y.left = element_text(vjust = 2),
    axis.ticks.length = unit(.25, "cm"),
    panel.grid = element_blank(),
    plot.margin = margin(30,30,20,20)
  )





Jd.d <- 
  M.data %>% 
  .[analysis %in% "Y",] %>% 
  .[!(TypeName.1 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>% 
  .[, julian.D_f := cut(julian.D,
                        breaks = c(seq(60,210,15)),
                        include.lowest = T)] %>% 
  .[, .(V1 = sum(Macaca_sur),.N), by= list(Year, Survey, julian.D_f)] %>% 
  .[, Encounter_rate := V1/N]   


ggplot(Jd.d, aes(julian.D_f, Encounter_rate))+
  geom_boxplot(size = 1, width = 0.4, fill= gray(.9),
               outlier.size = 3) +
  theme_bw() + 
  scale_x_discrete("Julian day",
                   labels = c("[60,75]",paste0(
                     "(",c(seq(75,180,15)),
                     ",",c(seq(90,180,15),182),
                     "]"
                   )))+
  theme(
    text = element_text(family="serif"),
    aspect.ratio = 1,
    panel.border = element_rect(size = 1.5,fill = NA),
    axis.line = element_line(size = 1, colour = "black"),
    axis.ticks = element_line(size = 1),
    axis.text = element_text(size = 18,colour = "black"),
    axis.text.x = element_text(angle = 315, hjust = -0.10),
    axis.title = element_text(size = 23,colour = "black",
                              vjust = -2, hjust = 0.5),
    axis.title.x.bottom = element_text(vjust = -2),
    axis.title.y.left = element_text(vjust = 2),
    axis.ticks.length = unit(.25, "cm"),
    panel.grid = element_blank(),
    plot.margin = margin(30,30,20,20)
  )




Type.d <- 
  M.data %>% 
  .[analysis %in% "Y",] %>% 
  .[!(TypeName.1 %in% "非森林"),] %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>%
  .[, TypeName.1:= ordered(TypeName.1, c("闊葉林", "針葉林", "混淆林", "竹林"))] %>% 
  .[, .(V1 = sum(Macaca_sur),.N), by= list(Year, Survey, TypeName.1)]   %>% 
  .[, Encounter_rate := V1/N]     


ggplot(Type.d, aes(TypeName.1, Encounter_rate))+
  geom_boxplot(size = 1, width = 0.4, fill= gray(.9),
               outlier.size = 3) +
  theme_bw() + 
  xlab("Forest type")+
  theme(
    text = element_text(family="serif"),
    aspect.ratio = 1,
    panel.border = element_rect(size = 1.5,fill = NA),
    axis.line = element_line(size = 1, colour = "black"),
    axis.ticks = element_line(size = 1),
    axis.text = element_text(size = 18,colour = "black"),
    axis.title = element_text(size = 23,colour = "black",
                              vjust = -2, hjust = 0.5),
    axis.title.x.bottom = element_text(vjust = -2),
    axis.title.y.left = element_text(vjust = 2),
    axis.ticks.length = unit(.25, "cm"),
    panel.grid = element_blank(),
    plot.margin = margin(30,30,20,20)
  )



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
