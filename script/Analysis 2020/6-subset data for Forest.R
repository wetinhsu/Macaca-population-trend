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

