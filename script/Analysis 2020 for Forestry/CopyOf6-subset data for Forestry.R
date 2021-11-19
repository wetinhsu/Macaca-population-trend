#樣點資料檢核
#---- load library

library(tidyverse)
library(readxl)
library(writexl)
library(sf)

#------------

M.data <- 
  read_excel("./data/clean/for analysis Forestrydata_2021_V1.xlsx",
             sheet="Data")  %>%
#  bind_rows(
#  read_excel("./data/clean/for analysis Forestrydata_V1.xlsx",
#                     sheet="Data"))  %>% 
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

M.data_notForest <- 
M.data %>%  
  filter(TypeName.1 %in% "非森林") %>% 
  mutate(cut = cut(Distance, breaks = seq(20,240,10),
                   include.lowest = T, right = TRUE))


#非森林的樣點離森林的距離
ggplot(M.data_notForest, aes(x = Distance))+
  geom_histogram(fill = gray(0.8), col = "black", breaks = seq(20,240,10))+
  stat_bin( geom="text", colour="black", size=3.5, breaks = seq(20,240,10),
           aes(label=..count.., y=(..count..)+0.6))+
  scale_x_continuous(breaks = seq(20,240,20),expand = c(0,0,0,5))+
  scale_y_continuous(expand = c(0,0,0,5))+
  labs(x = "Distance", y = "Count")+
  theme(
    text = element_text(family="serif"),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_text(size = 14,colour = "black"),
    axis.title = element_text(size = 18,colour = "black",
                              vjust = -2, hjust = 0.5)
  )
 


#----------
#林管處的Encounter_rate
M.data %>% 
  filter(analysis %in% "Y") %>% 
  group_by(Office, Survey, Year) %>% 
  summarise(E = sum(Macaca_sur)/n()) %>% 
  
  ggplot(., aes(x = Office, y = E)) +
  geom_boxplot(size = 1, width = 0.4, fill= gray(.9))+
  scale_y_continuous(breaks = seq(0,0.15,0.05), limits = c(0,0.15))+
  labs(x = "林管處", y = "Encounter_rate")+
  theme(
    text = element_text(family="serif"),
    panel.border = element_rect(size = 1.5,fill = NA),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_text(size = 14,colour = "black"),
    axis.title = element_text(size = 18,colour = "black",
                              vjust = -2, hjust = 0.5)
  )

  
#-----------
#TypeName.1的Encounter_rate
M.data %>% 
  filter(analysis %in% "Y") %>% 
  group_by(TypeName.1, Survey, Year) %>% 
  summarise(E = sum(Macaca_sur)/n()) %>% 
  
  ggplot(., aes(x = TypeName.1, y = E)) +
  geom_boxplot(size = 1, width = 0.4, fill= gray(.9))+
  scale_y_continuous(breaks = seq(0,0.15,0.05), limits = c(0,0.15))+
  labs(x = "森林類型", y = "Encounter_rate")+
  theme(
    text = element_text(family="serif"),
    panel.border = element_rect(size = 1.5,fill = NA),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_text(size = 14,colour = "black"),
    axis.title = element_text(size = 18,colour = "black",
                              vjust = -2, hjust = 0.5)
  )

#-----------
#Altitude的Encounter_rate
Alt.d.n <-
  M.data %>% 
  filter(analysis %in% "Y") %>% 
  mutate(Altitude_f =cut(Altitude,
                         breaks = c(seq(0,4000,500)),
                         labels = c(seq(250,3750,500)),
                         include.lowest = T) ) %>% 
  group_by(Altitude_f,Survey, Year) %>% 
  summarise(N = n(), E = sum(Macaca_sur)/n()) %>% 
  group_by(Altitude_f) %>% 
  summarise(mean_N = mean(N),  y = quantile(E,0.75)) 

  M.data %>% 
  filter(analysis %in% "Y") %>% 
  mutate(Altitude_f =cut(Altitude,
                         breaks = c(seq(0,4000,500)),
                         labels = c(seq(250,3750,500)),
                         include.lowest = T) ) %>% 
  group_by(Altitude_f,Survey, Year) %>% 
  summarise(E = sum(Macaca_sur)/n()) %>% 
  
  ggplot(., aes(x = Altitude_f, y = E)) +
  geom_boxplot(size = 1, width = 0.4, fill= gray(.9))+
    geom_text(data = Alt.d.n, aes(y = y+0.0025,label = paste0(mean_N,c("","","","","","",""))),
              size = 4,
              hjust = -0.1,
              position = position_dodge(0.9))+
    
  scale_y_continuous(breaks = seq(0,0.10,0.02), limits = c(0,0.10))+
  labs(x = "Elevation (m)", y = "Encounter rate (troop/point)") +
  theme(
    text = element_text(family="serif"),
    panel.border = element_rect(size = 1.5,fill = NA),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_text(size = 14,colour = "black"),
    axis.title = element_text(size = 18,colour = "black",
                              vjust = -2, hjust = 0.5)
  )


  M.data %>% 
    filter(analysis %in% "Y") %>% 
    mutate(Altitude_f =cut(Altitude,
                           breaks = c(seq(0,4000,500)),
                           labels = c(seq(250,3750,500)),
                           include.lowest = T) ) %>% 
    group_by(Altitude_f,Survey, Year) %>% 
    summarise(E = sum(Macaca_sur)/n()) %>% 
    group_by(Altitude_f) %>% #確認中位數
    summarise(mid = median(E))

  
  
  
  M.data %>% 
    filter(analysis %in% "Y") %>% 
    mutate(Altitude_f =cut(Altitude,
                           breaks = c(seq(0,4000,50)),
                           labels = c(seq(25,4000,50)),
                           include.lowest =T) ) %>% 
    group_by(Altitude_f, Year) %>% 
    summarise(N = n()) %>% 
 #   mutate(Altitude_f = Altitude_f %>% as.character() %>% as.numeric()) %>% 
    ggplot(., aes(x = Altitude_f, y = N))+
    geom_point()
  
  