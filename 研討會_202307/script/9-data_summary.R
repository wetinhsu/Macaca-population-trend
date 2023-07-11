library(data.table)
library(tidyverse)
library(readxl)
library(writexl)
library(here)
#---------------------------


df <- read_excel("研討會_202307/有林務局資料/data/clean/for analysis_1522_v1.xlsx",
                 col_types = "text") 
#全部的樣區數
df%>%
  summarise(
    length = Site_N %>% unique() %>% length
  ) 

#每次的樣區數
df%>%
  group_by(Year,  Survey) %>% 
  summarise(
    n_Site_N = Site_N %>% unique() %>% length,
  ) %>% 
  summary()

tb1 <- 
df%>%
  group_by(Year) %>% 
  summarise(
    n_Site_N = Site_N %>% unique() %>% length
  ) 

  ggplot(data = tb1, aes(x = Year, y =  n_Site_N)) +
    geom_col(width = 0.5)+
    scale_y_continuous("Count of Transects")+
  theme_classic()


#每次有猴群的樣區數

df%>%
  filter(Macaca_sur == 1) %>% 
  group_by(Year, Site_N, Survey) %>% 
  summarise(NN = Site_N %>% unique() %>% length) %>% 
  group_by(Year, Survey) %>%
  summarise(NN_survey = NN %>% as.numeric() %>% sum) %>% 
  summary




#總猴群數
df%>%
  filter(Macaca_sur == 1) %>% 
  summarise(NN = Macaca_sur %>% length)


#平均每次調查的猴群數
df%>%
  filter(Macaca_sur == 1) %>% 
  group_by(Year, Survey) %>%
  summarise(NN = Macaca_sur %>% length) %>% 
  summary

#-------------------
mmm <- 
df%>%
  filter(Macaca_sur == 1) %>% 
  filter(!(Year %in% 2020 & Survey %in% 3)) %>% 
  select(Site_N, Year,Survey) %>% 
  unique() %>% 
  reshape2::dcast(Site_N ~ Year, length) %>% 
  mutate(total = `2015`+`2016`+`2017`+`2018`+`2019`+`2020`+`2021`+`2022`) 


mmm$total %>% table #猴子的聚集地A33-08、A18-01 
#其實不是我們想像中出現猴群的就是那幾個樣區

#-------------

Surveyer <- 
  df %>% 
  select(Year, Site_N, Point,  Survey,Surveyor) %>% 
  unique() %>%
  
  separate(.,Surveyor,
           into = paste0("Surveyor","_",0:10),
           sep ="、", extra = "drop", fill = "right") %>% 
  reshape2::melt(.,id.vars = c("Year", "Site_N", "Point",  "Survey"),
                 variable.name = "Surveyer", value.name = "Name",) %>% 
  filter(!is.na(Name)) %>% 
  select(-Point) %>% 
  unique(.) 


#每次調查者人數
Surveyer %>% 
  
  group_by(Year, Survey) %>% 
  summarise(NN=Name %>% unique() %>% length) %>% 
  summary

#總調查者人數
Surveyer %>% 
  summarise(NN=Name %>% unique() %>% length) 


tb2 <- 
Surveyer %>% 
  group_by(Year) %>% 
  summarise(NN=Name %>% unique() %>% length) 

#----------------------------------------------------------------

df %>% 
  group_by(Year, Site_N, Survey) %>% 
  summarise(NN = Point %>% unique %>% length) %>% 
  .$NN %>% 
  table

#2015~2022年間樣區被調查的次數的統計
df %>% 
  group_by(Year, Site_N, Survey) %>% 
  summarise(NN = Site_N %>% unique()%>% length() ) %>% 
  group_by(Site_N) %>% 
  summarise(Count = NN %>% length() ) %>% 
  
  .$Count %>%  
    table
#-------------------------------------------------




df %>% 
  group_by(Site_N) %>% 
  summarise(Count=  Year %>% unique %>% length() ) %>% 
  filter(Count>=3) %>% View
    
  
df %>% 
  select(Site_N,Year, DataSource) %>%
  mutate(Year = as.character(Year)) %>% 
  unique() %>% 
  reshape2::melt(id = c("Site_N", "Year")) %>% 
  reshape2::dcast(Site_N+Year~ value)%>%
#  View

  write.csv(.,"研討會_202307/有林務局資料/data/clean/BBS&Forest.csv", row.names = F)


inner_join(df[df$DataSource == "Foresty",], df[df$DataSource == "BBS",], by = c("Site_N")) %>% 
  select(Site_N) %>% unique() %>% 

  View
