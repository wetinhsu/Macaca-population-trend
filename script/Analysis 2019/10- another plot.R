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
  .[!(TypeName.1 %in% "非森林"),] %>% 
  .[Macaca_sur %in%1,] %>%
  .[, .N]


#---------------------
#各樣點重複調查的次數，目前最多10次

Times_survey_50 <- 
M.data %>% 
  .[analysis %in% "Y",] %>% 
  .[!(TypeName.1 %in% "非森林"),] %>%
  mutate(SP = paste0(Site_N, "-", Point)) %>% 
  group_by(SP) %>%
  summarise(N = n()) %>% 
  group_by(N) %>% 
  summarise(count = n()) %>% 
  mutate(analysis = "Y") %>% 
  mutate(percent = round(count/sum(count)*100))

Times_survey <- 
M.data %>% 
 # .[analysis %in% "Y",] %>% 
 # .[!(TypeName.1 %in% "非森林"),] %>%
  mutate(SP = paste0(Site_N, "-", Point)) %>% 
  group_by(SP) %>%
  summarise(N = n()) %>% 
  group_by(N) %>% 
  summarise(count = n()) %>% 
  mutate(analysis = "N") %>% 
  mutate(percent = round(count/sum(count)*100))


M.data %>% 
  .[analysis %in% "Y",] %>% 
  .[!(TypeName.1 %in% "非森林"),] %>%
  mutate(SP = paste0(Site_N, "-", Point)) %>% 
  dim()

M.data %>% 
  #.[analysis %in% "Y",] %>% 
 # .[!(TypeName.1 %in% "非森林"),] %>%
  mutate(SP = paste0(Site_N, "-", Point)) %>% 
  dim()


rbind(Times_survey_50, Times_survey) %>% 
  mutate(analysis = ordered(analysis , c("Y", "N"))) %>% 
  ggplot(., aes(x = N, y = count, fill = analysis))+
  geom_bar(position = "dodge", stat="identity", col = "black")+
  
  geom_text(aes(x =N, label = count),
            position = position_dodge(1), size = 3 , vjust = -0.5, color=I("red"))+
  
  scale_fill_manual(values = c("Y" = "black", 
                               "N" = "white"),
                    
                    breaks = c("Y", 
                               "N"),
                    
                    labels = c("Elevation >50 m (n = 2014)",
                               "All (n = 3844)"),
                    name = "")+
  
  scale_x_continuous("Times",breaks = c(1:10))+
  scale_y_continuous("Counts")+
  theme_bw()+
  
  theme(
    axis.title = element_text(size = 17),
    axis.text = element_text(size = 16),
    text = element_text(family="serif"),
    legend.position = c(0.05, 0.99),
    legend.justification = c("left", "top"),
    legend.text = element_text(size = 12)
    
  )
ggsave("./result/各樣點的調查次數.jpg")

rbind(Times_survey_50, Times_survey) %>% 
  mutate(analysis = ordered(analysis , c("Y", "N"))) %>%  
  ggplot(., aes(x = N, y = percent, fill = analysis))+
  geom_bar(position = "dodge",stat="identity", col = "black")+
  
  geom_text(aes(x =N, label = percent),
            position = position_dodge(1), size = 4 , vjust = -0.5, color=I("red"))+
  
  scale_fill_manual(values = c("Y" = "black", 
                               "N" = "white"),
                    
                    breaks = c("Y", 
                               "N"),
                    
                    labels = c("Elevation >50 m (n = 2014)",
                               "All (n = 3844)"),
                    name = "")+
  
  scale_x_continuous("Times",breaks = c(1:10))+
  scale_y_continuous("Percent (%)")+
  theme_bw()+

  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    text = element_text(family="serif"),
    legend.position = c(0.05, 0.99),
    legend.justification = c("left", "top"),
    legend.text = element_text(size = 12)

  )
ggsave("./result/各樣點的調查次數比例.jpg")

#---

M.data %>% 
  .[analysis %in% "Y",] %>% 
  .[!(TypeName.1 %in% "非森林"),] %>%
  .[, Altitude_f := cut(Altitude,
                        breaks = c(seq(0,4000,500)),
                        labels = c(seq(250,3750,500)),
                        include.lowest = T)]%>%
  mutate(Region2 =  ordered(Region2,  
                            levels = c("North", "Center1", "Center2", "South", "East1", "East2"),
                            labels = c("Northern", "Central", "Southwestern", "Southern", "Hualien", "Taitung"))) %>% 
  
  ggplot(., aes(x = Altitude_f))+
  geom_histogram(stat="count")+
  facet_grid(Region2~., scales = "free_y") 

M.data %>% 
  .[analysis %in% "Y",] %>% 
  .[!(TypeName.1 %in% "非森林"),] %>%
  mutate(Region2 =  ordered(Region2,  
                            levels = c("North", "Center1", "Center2", "South", "East1", "East2"),
                            labels = c("Northern", "Central", "Southwestern", "Southern", "Hualien", "Taitung"))) %>% 
  
  ggplot(., aes(x = TypeName.1))+
  geom_histogram(stat="count")+
  facet_grid(Region2~., scales = "free_y") 

#-----

M.data %>% .[analysis %in% "Y",] %>% 
  .[!(TypeName.1 %in% "非森林"),] %>% 
  .[, Macaca_sur := ifelse(is.na(Macaca_sur),0, Macaca_sur)] %>% 
  split(., .$Region2) %>% 
  lapply(., function(x){

    replicate(10000, mean(sample(x$Macaca_sur, size = 635, replace = TRUE))) %>% quantile(.,probs = c(0.025, 0.975)) 
  }) %>% 
  bind_rows(.id = "Region2")



M.data %>% .[analysis %in% "Y",] %>% 
  .[!(TypeName.1 %in% "非森林"),] %>% 
  .[, Macaca_sur := ifelse(is.na(Macaca_sur),0, Macaca_sur)] %>% 
  split(., .$Region2) %>% 
  lapply(., function(x){
    mean = replicate(10000, mean(sample(x$Macaca_sur, size = 635, replace = TRUE))) %>% mean
  }) %>% 
  bind_rows(.id = "Region2") 


#-----------------------------

#---- load library

library(data.table)
library(magrittr)
library(readxl)
library(tidyr)
library(dplyr)
library(writexl)

#------------------------
#2015-2017

Surveyer.1517 <- 
  lapply(paste0("./data/raw/BBSdata/", 2015:2017), function(x){
    list.files(x, pattern = "BBSdata_", full.names = T) %>%  
      read_xlsx(., sheet = "birddata", cell_cols("D:AF")) %>% 
      setDT %>%
      #.[時段 %in% c("0-3minutes", "3-6minutes"),] %>%
      .[!(時段 %in% "Supplementary"),] %>%
      .[調查旅次編號 %in% c(1,2)] %>%
      .[ !分析 %in% "N", list(年, 樣區編號, 樣點編號, 調查旅次編號, 調查者)] %>%
      setnames(., c("Year", "Site_N", "Point",  "Survey", "Surveyer")) %>%
      .[!duplicated(.)]  
  } ) %>% 
  do.call(rbind, .) %>%
  
  separate(.,Surveyer,
           into = c("Surveyer_0","Surveyer_1","Surveyer_2","Surveyer_3"),
           sep ="、", extra = "drop", fill = "right") %>% 
  reshape2::melt(.,id.vars = c("Year", "Site_N", "Point",  "Survey"),
                 variable.name = "Surveyer", value.name = "Name",) %>% 
  filter(!is.na(Name)) %>% 

  unique(.) 

#2018

Surveyer.18 <- 
  lapply(paste0("./data/raw/BBSdata/", 2018), function(x){
    list.files(x, pattern = "BBSdata_", full.names = T) %>%  
      read_xlsx(., sheet = "birddata", cell_cols("B:AF"),col_types ="text") %>% 
      setDT %>%
      #.[時段 %in% c("0-3minutes", "3-6minutes"),] %>%
      .[!(時段 %in% "Supplementary"),] %>%
      .[調查旅次編號 %in% c(1,2)] %>%
      .[ !分析 %in% "N", list(年, 樣區編號, 樣點編號, 調查旅次編號, 調查者)] %>%
      setnames(., c("Year", "Site_N", "Point",  "Survey", "Surveyer")) %>%
      .[!duplicated(.)]  
  } ) %>% 
  do.call(rbind, .)%>%
  
  separate(.,Surveyer,
           into = paste0("Surveyer","_",0:10),
           sep ="、", extra = "drop", fill = "right") %>% 
  reshape2::melt(.,id.vars = c("Year", "Site_N", "Point",  "Survey"),
                 variable.name = "Surveyer", value.name = "Name",) %>% 
  filter(!is.na(Name)) %>% 

  unique(.) 

#2019

Surveyer.19 <- 
  lapply(paste0("./data/raw/BBSdata/", 2019), function(x){
    list.files(x, pattern = "BBSdata_", full.names = T) %>%  
      read_xlsx(., sheet = "birddata", cell_cols("B:AF")) %>% 
      setDT %>%
      #.[時段 %in% c("0-3minutes", "3-6minutes"),] %>%
      .[!(時段 %in% "Supplementary"),] %>%
      .[調查旅次編號 %in% c(1,2)] %>%
      .[ !分析 %in% "N", list(年, 樣區編號, 樣點編號, 調查旅次編號, 調查者)] %>%
      setnames(., c("Year", "Site_N", "Point",  "Survey", "Surveyer")) %>%
      .[!duplicated(.)]  
  } ) %>% 
  do.call(rbind, .)%>%
  
  separate(.,Surveyer,
           into = paste0("Surveyer","_",0:10),
           sep ="、", extra = "drop", fill = "right") %>% 
  reshape2::melt(.,id.vars = c("Year", "Site_N", "Point",  "Survey"),
                 variable.name = "Surveyer", value.name = "Name",) %>% 
  filter(!is.na(Name)) %>% 

  unique(.) 



Name.list <- 
  rbind(Surveyer.1517,
        Surveyer.18,
        Surveyer.19) %>% 
  unique(.) %>% 
  
  mutate(Name = gsub("*.(老鳥.*).*|*.(幕後.*).*", "",Name)) %>% 
  mutate(Name = gsub("Tefon", "吳杰峰",Name)) %>%
  mutate(Name = gsub("呂效修", "呂効修",Name)) %>%
  mutate(Name = gsub("劉晉笠", "劉晉岦",Name)) %>%
  mutate(Name = gsub("黃淳禛", "黃淳禎",Name)) %>%
  mutate(Name = gsub("簡美棋", "簡美祺",Name)) %>%
  mutate(Name = gsub("魏硯昀", "魏硯畇",Name)) %>%
  filter(! Name %in% "大安社大學員6人")

Name.list %>% 
  group_by(Year, Survey) %>% 
  summarise(N = Name %>% unique %>% length) %>% 
  mutate(Y_S = paste0(Year, "_", Survey)) %>% 
  
  ggplot(., aes(x = Y_S, y = N))+
  geom_bar(position = "dodge",stat="identity", col = "black", fill = gray(0.7), width = 0.7)+
  
  geom_text(aes(x = Y_S, label = N),
            position = position_dodge(1), size = 4 , vjust = -0.5, color=I("red"))+
  
  
  labs(x = "Year_Survey", y = "Count")+
  ylim(0,400)+
  
  theme_bw()+
  
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 11),
    text = element_text(family="serif"),
    legend.position = c(0.05, 0.95),
    legend.justification = c("left", "top")
    
  )

ggsave("./result/各旅次的調查樣點數.jpg")




Times_surveyer <- 
Name.list %>% 
  select(Year, Survey, Name) %>% 
  unique() %>%
  group_by(Name) %>%
  summarise(N = n()) %>% 
  group_by(N) %>% 
  summarise(count = n()) 
 
ggplot(Times_surveyer, aes(x = N, y = count))+
  geom_bar(position = "dodge",stat="identity", col = "black", fill = gray(0.7))+
  
  geom_text(aes(x =N, label = count),
            position = position_dodge(1), size = 4 , vjust = -0.5, color=I("red"))+

  
  scale_x_continuous("Times",breaks = c(1:10))+
  scale_y_continuous("Count")+
  theme_bw()+
  
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    text = element_text(family="serif"),
    legend.position = c(0.05, 0.95),
    legend.justification = c("left", "top")
    
  )

ggsave("./result/各調查者的調查次數.jpg")

#--------
count_siteAndsurveyer <- 
Name.list %>% 
  group_by(Year) %>% 
  summarise(site_n = Site_N %>% unique() %>% length(),
            name_n = Name %>% unique() %>% length()) 


 
  ggplot(data = count_siteAndsurveyer ,aes(Year, site_n))+
    geom_bar(aes( fill = "gray"),position = "dodge",stat="identity",col = "black", width = 0.5)+
    geom_line(aes(x = Year, y = name_n, group = F), size = 1)+
    geom_point(aes(x = Year, y = name_n, group = F, col = "black"), size = 5)+
    
    labs(x = "Year", y = "Count")+
    scale_y_continuous("Count", breaks = seq(0, 400,100), limits = c(0,400))+
    
    scale_color_manual(values = c("black"),
                       labels = c("black"))+
    scale_fill_manual(values = c("gray"),
                      labels = c("gray"))+

    
    theme_bw()+    
    guides(colour = guide_legend(order = 1, title = "" ,override.aes = list(size = 2)),
           fill = guide_legend(order = 2, title = ""))+
    theme(
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 16),
      text = element_text(family="serif"),
      legend.position = c(0, 1),
      legend.background = element_blank(),
      legend.justification = c("left", "top"),
      legend.box = "horizontal"
      
    )

  
  Name.list %>% 
    select(Site_N) %>% 
    unique %>% 
    dim
  