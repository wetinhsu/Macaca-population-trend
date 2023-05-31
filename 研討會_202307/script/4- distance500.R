library(tidyverse)
library(readxl)
library(tidyr)
library(writexl)
library(sf)
library(here)


here::here() 



M.data <- read_excel(here("./研討會_202307/data/clean/merge_data_1522.xlsx") ) %>% 
  mutate(Macaca_sur.o = Macaca_sur) %>% 
  mutate(Macaca_sur = ifelse(Macaca_sur %in% 1 & Macaca_dist %in% "C" , NA, Macaca_sur)) 



M.data <- 
  M.data %>%  
  reshape2::dcast(Year + Site_N ~ Survey, length, value.var = "Macaca_sur") %>% 
  #計算第1旅次及第2旅次調查的樣點數
  filter(`1` %in% 0 ) %>%  #找出有第2旅次沒第1旅次的樣區
  left_join(M.data, ., by = c("Year", "Site_N"))%>%
  mutate( Survey = ifelse(!is.na(`1`),1, Survey)) %>% #將第1次調查的旅次改回1
  select(-`1`, -`2`, -`3`)

M.0<- M.data %>%
  filter(Macaca_sur %in% 1)  #猴群

M.2<- M.0 %>%  #找出同一旅次同一樣區有猴群大於2者
  group_by(Year, Survey, Site_N) %>% 
  summarise(N = n()) %>% 
  filter(N>1)

M.abc <- M.2 %>%
  right_join(M.0,., by = c("Year", "Survey","Site_N")) %>% 
  mutate( Y_S_S = paste0(Year, "_", Survey, "_",Site_N)) %>% 
  mutate_at(c("X_97", "Y_97"),as.integer) %>% 
  as.data.frame() %>% 
  st_as_sf(., coords = c("X_97", "Y_97"), crs = 3826) %>% 
  
  split(., .$Y_S_S) %>% 
  lapply(., function(x){
    
    dist <-   
      st_distance(x$geometry)    #算點與點間的距離
    
    colnames(dist) <- x$Point         #加colnames
    
    dist %>% 
      as.data.frame() %>%   #轉回長表格
      setNames(., x$Point) %>% 
      add_column(Base_point = x$Point) %>% 
      reshape2::melt(id.vars = "Base_point",
                     variable.name = "Nearest_point",
                     value.name = "distance")%>%  
      filter(!distance %in% 0) %>% 
      arrange(Base_point)
  }) %>% 
  bind_rows(.id = "Y_S_S") %>% 
  separate("Y_S_S", c("Year", "Survey","Site_N"), "_") %>% 
  mutate(distance = as.numeric(distance)) %>% 
  mutate(Nearest_point = Nearest_point %>% as.character() %>% as.numeric()) %>% 
  mutate(Base_point = as.numeric(Base_point))

#要刪除的清單
remove.data <- 
  M.abc %>%  
  filter(distance<500)%>% 
  mutate( Y_S_S = paste0(Year, "_", Survey, "_",Site_N)) %>% 
  split(., .$Y_S_S) %>% 
  lapply(., function(x){
    
    if(nrow(x)==2){
      x <- x[2,]  #要刪除第2點
    }else{
      x  #2點以上，留下來再討論，優先考量留下距離段近的，較遠的刪除
    }
  }) %>% 
  bind_rows %>% 
  # 手動
  filter(!( Year %in% 2020 & Survey %in% 2 & Site_N %in% "A21-02" & Base_point %in% c(3))) %>% 
  filter(!( Year %in% 2015 & Survey %in% 2 & Site_N %in% "A29-17" & Base_point %in% c(3,6))) %>%
  filter(!( Year %in% 2015 & Survey %in% 1 & Site_N %in% "A33-08" & Base_point %in% c(2))) %>%
  filter(!( Year %in% 2016 & Survey %in% 2 & Site_N %in% "A33-08" & Base_point %in% c(2,5))) %>%
  filter(!( Year %in% 2019 & Survey %in% 2 & Site_N %in% "A33-08" & Base_point %in% c(3,9))) %>%
  filter(!( Year %in% 2020 & Survey %in% 2 & Site_N %in% "A33-08" & Base_point %in% c(2,6))) %>%
  filter(!( Year %in% 2015 & Survey %in% 2 & Site_N %in% "A33-28" & Base_point %in% c(5,7))) %>%
  filter(!( Year %in% 2019 & Survey %in% 2 & Site_N %in% "A33-32" & Base_point %in% c(4,7))) %>%
  filter(!( Year %in% 2018 & Survey %in% 1 & Site_N %in% "A35-15" & Base_point %in% c(2,7))) %>%
  filter(!( Year %in% 2020 & Survey %in% 2 & Site_N %in% "A35-15" & Base_point %in% 2)) %>%
  filter(!( Year %in% 2022 & Survey %in% 2 & Site_N %in% "A33-08" & Base_point %in% c(8))) %>%
  # ~手動
  select( -Nearest_point, -distance, -Y_S_S) %>% 
  unique()%>% 
  setNames(., c("Year", "Survey","Site_N", "Point")) %>% 
  mutate(Point = as.character(Point))

M.data.1 <-
  #remove 重複的猴群
  M.data %>% 
  semi_join(remove.data) %>% 
  mutate(Macaca_sur = NA)%>% 
  bind_rows(.,
            anti_join(M.data, remove.data) ) %>% 
  arrange(Year, Survey, Site_N, Point)   
  
  #~~remove 重複的猴群



M.data %>%  #移除重複前
  filter(Macaca_sur %in% 1) %>% 
  group_by(Year, Survey, Site_N) %>% 
  summarise(N = n()) %>%
  .$N %>% 
  table 


M.data.1 %>%   #移除重複後  #樣區數應該不會變
  filter(Macaca_sur %in% 1) %>% 
  group_by(Year, Survey, Site_N) %>% 
  summarise(N = n()) %>% 
  .$N %>% 
  table

M.0 %>%  
  group_by(Year, Survey, Site_N) %>% 
  summarise(N = n()) %>% 
  reshape2::dcast(Site_N ~ Year+Survey,
                  value.var ="N",
                  fill="") 

write_xlsx(M.data.1, here("./研討會_202307/data/clean/Distance500_1522.xlsx"))
