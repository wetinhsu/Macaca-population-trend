#樣點資料檢核
#---- load library

library(tidyverse)
library(readxl)
library(writexl)
library(sf)

#------------

M.data <- read_excel("./data/clean/for analysis Forestrydata_V1.xlsx",
                     sheet=1)  %>% 
  mutate(Office = ordered(Office, c("羅東", "新竹", "東勢", "南投", "嘉義", "屏東", "花蓮", "臺東"))) %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(Survey = as.numeric(Survey)) %>% 
  mutate(Point = as.numeric(Point)) %>% 
  mutate(Month = as.numeric(Month)) %>% 
  mutate(Day = as.numeric(Day)) %>% 
  mutate(Macaca_sur = as.numeric(Macaca_sur)) %>% 
  mutate(Distance = as.numeric(Distance)) 
  
  
#-----------------------------------

(Group_than2<- M.data %>%
   filter(analysis %in% "Y") %>% 
   filter(!(TypeName.1 %in% "非森林")) %>% 
   filter(Macaca_sur %in% 1) %>% 
   group_by(Year, Survey, Site_N) %>% 
     summarise(N = n()) %>% 
   filter(N>1)

)




(Composition_Forest<- M.data %>% 
   filter(analysis %in% "Y") %>% 
   filter(!(TypeName.1 %in% "非森林")) %>% 
   group_by(TypeName.1,TypeName) %>% 
   summarise(N = n(), m = sum(Macaca_sur)))



(E_rate_Office<-
    M.data %>% 
    filter(analysis %in% "Y") %>% 
    filter(!(TypeName.1 %in% "非森林")) %>%
    group_by(Year, Survey, Office) %>%
    summarise(N = n(),
              m = sum(Macaca_sur)) %>% 
    bind_rows(group_by(.,Year, Survey) %>%  #增加total的部分
                summarise(N=sum(N), m = sum(m)) %>%
                mutate(Office='Total')) %>% 
    mutate(E = m/N) %>%
    ungroup() %>%
  
    group_by(Office) %>%
    summarise(Mean_N = mean(N),
              Se_N = sd(N)/sqrt(n()),
              Mean_m = mean(m),
              Se_m = sd(m)/sqrt(n()),
              Mean_E = mean(E),
              Se_E = sd(E)/sqrt(n())) 
)


tmp.forest <- 
M.data %>%  #非森林的小計
  filter(TypeName.1 %in%　"非森林") %>% 
  group_by(Year, Survey, TypeName.1) %>% 
  summarise(N = n(),
            m = sum(Macaca_sur)) 

tmp.alt <- 
M.data %>%  #<50m & >=50m 分別森林的小計
  filter(!TypeName.1 %in%　"非森林") %>%
  mutate(TypeName.1 = ifelse(Altitude<50, "less50","Forest")) %>%
  group_by(Year, Survey, TypeName.1) %>% 
  summarise(N = n(),
            m = sum(Macaca_sur)) 

(E_rate_Forest<-
M.data %>% 
  group_by(analysis, Year, Survey, TypeName.1) %>%
  summarise(N = n(),
            m = sum(Macaca_sur)) %>%  
  bind_rows(group_by(.,Year, Survey) %>% #增加total的部分，total的資料包含非森林及<50m
              summarise(N=sum(N), m = sum(m)) %>%
              mutate(TypeName.1='Total', analysis ="Y")) %>% 
  filter(analysis =="Y") %>%   #移除 非森林及<50m
  select(-analysis)%>% 
  
  bind_rows(., tmp.alt, tmp.forest) %>%  #增加是先算好 森林、非森林、<50的小計
  
  mutate(E = m/N) %>%
  ungroup() %>%
  
  group_by(TypeName.1) %>%
  summarise(Mean_N = mean(N),
            Se_N = sd(N)/sqrt(n()),
            Mean_m = mean(m),
            Se_m = sd(m)/sqrt(n()),
            Mean_E = mean(E),
            Se_E = sd(E)/sqrt(n())) 
)





write_xlsx(list(
  "大於2群的樣區" = Group_than2,
  "森林組成" = Composition_Forest,
  "rate_林管處" = E_rate_Office,
  "rate_森林" = E_rate_Forest
),
paste0("./林務局報告_20201013/tables_",format(Sys.Date(),"%y%m%d"),".xlsx"))


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

