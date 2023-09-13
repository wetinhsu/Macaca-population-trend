library(tidyverse)
library(readxl)
library(writexl)
library(sf)
library(here)

library(flextable)
library(ftExtra)
library(showtext)

showtext_auto()
font_add("Microsoft JhengHei", "msjh.ttc")
here::here() 
#---
M.data <- 
  here("./data/clean/Forestry/for analysis/") %>% 
  list.files(., full.names = T) %>% 
  str_subset(paste0(2020:2022)) %>% #如果只要2020~2022年，就寫2020:2022
  lapply(., read_excel, sheet="Data", col_types = "text") %>% 
  bind_rows() %>% 
  
  
  mutate(Office = ordered(Office, 
                          c("羅東", "新竹", "東勢", "南投",
                            "嘉義", "屏東", "花蓮", "臺東"))) %>% 
  mutate(TypeName.1 = ordered(TypeName.1,
                              c("闊葉林", "針葉林", "竹林",
                                "混淆林", "非森林"))) %>% 
  mutate_at(c("Year", "Survey","Month",
              "Day", "Macaca_sur", "Distance", "Altitude", "julian.D"), as.numeric) 

#------
Office.d.n <-
  M.data %>% 
  #  filter(Year == 2022) %>% 
  filter(analysis %in% "Y") %>% 
  group_by(Office,Survey, Year) %>% 
  summarise(N = n(), E = sum(Macaca_sur)/n()) %>% 
  
  group_by(Office) %>% 
  summarise(mean_N = mean(N) %>% round(1), 
            E25 = quantile(E,0.25),
            E50 = quantile(E,0.50),
            E75 = quantile(E,0.75)) %>% 
  ungroup()



M.data %>% 
  #  filter(Year == 2022) %>% 
  filter(analysis %in% "Y") %>% 
  group_by(Office, Survey, Year) %>% 
  summarise(E = sum(Macaca_sur)/n()) %>% 
  mutate(Year = as.character(Year)) %>% 
  
  ggplot(., aes(x = Office, y = E)) +
  geom_boxplot(width = 0.4, linewidth = 0.4, fill= gray(.9))+
  geom_text(data = Office.d.n, 
            aes(y = E75,
                label = mean_N),
            size = 4.5,
            hjust = -0.1, 
            vjust = -1,
            position = position_dodge(0.9), show.legend = F)+
  scale_y_continuous(breaks = seq(0,0.15,0.05), limits = c(0,0.15))+
  scale_x_discrete(limits = c( "新竹", "東勢", "南投","羅東",
                               "嘉義", "屏東", "花蓮", "臺東"),
                   labels = c( "新竹", "臺中", "南投","宜蘭",
                               "嘉義", "屏東", "花蓮", "臺東")
  )+
  labs(x = "", y = "相對密度(群/樣點)")+
  theme(
    text = element_text(family="Microsoft JhengHei"),
    panel.border = element_rect(size = 1.5,fill = NA),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_text(size = 20,colour = "black"),
    axis.title = element_text(size = 18,colour = "black"),
    axis.title.y = element_text(angle = 270,
                              vjust = -1, hjust = 0.5),
    legend.position = "top"
  )
ggsave(here("./Report of Foresty_20221024/Office2.PNG"),
       dpi = 300,
       width = 895,
       height = 879,
       units = "px")


#----------

Year.d.n <-
  M.data %>% 
  #  filter(Year == 2022) %>% 
  filter(analysis %in% "Y") %>% 
  group_by(Survey, Year) %>% 
  summarise(N = n(), E = sum(Macaca_sur)/n()) %>% 
  
  group_by(Year) %>% 
  summarise(mean_N = mean(N) %>% round(1), 
            E25 = quantile(E,0.25),
            E50 = quantile(E,0.50),
            E75 = quantile(E,0.75)) %>% 
  ungroup()



M.data %>% 
  #  filter(Year == 2022) %>% 
  filter(analysis %in% "Y") %>% 
  group_by(Survey, Year) %>% 
  summarise(E = sum(Macaca_sur)/n()) %>% 
  mutate(Year = as.character(Year)) %>% 
  
  ggplot(., aes(x = Year, y = E)) +
  geom_boxplot(width = 0.3, linewidth = 0.4, fill= gray(.9))+
  geom_text(data = Year.d.n, 
            aes(x = 1:3,
                y = E75,
                label = mean_N),
            size = 4,
            hjust = -0.2, 
            vjust = -1,
            position = position_dodge(0.9), show.legend = F)+
    scale_y_continuous(breaks = seq(0,0.1,0.02), limits = c(0,0.1))+
  labs(x = "", y = "相對密度(群/樣點)")+
  theme(
    text = element_text(family="Microsoft JhengHei"),
    panel.border = element_rect(size = 1.5,fill = NA),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_text(size = 20,colour = "black"),
    axis.title = element_text(size = 18,colour = "black"),
    axis.title.y = element_text(angle = 270,
                                vjust = -1, hjust = 0.5),
    legend.position = "top"
  )

ggsave(here("./Report of Foresty_20221024/YEAR2.PNG"),
       dpi = 300,
       width = 895,
       height = 879,
       units = "px")
