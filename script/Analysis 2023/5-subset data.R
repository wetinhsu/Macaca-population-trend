library(tidyverse)
library(readxl)
library(tidyr)
library(writexl)
library(sf)
library(here)


here::here() 



M.data <- read_excel(here("./data/clean/full_combind_data_1523.xlsx") ) %>% 
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
  left_join(M.0) %>% 
  mutate( Y_S_S = paste0(Year, "_", Survey, "_",Site_N)) %>% 
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
  filter(distance<300)%>% 
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
  filter(!( Year %in% 2020 & Survey %in% 2 & Site_N %in% "A35-15" & Base_point %in% 2)) %>% 
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
  arrange(Year, Survey, Site_N, Point)%>%   
  
  #~~remove 重複的猴群
  filter(!grepl("KIN", .$Site_N, perl = FALSE, fixed = FALSE, )) %>%   #exculde kiman
  filter(!(Site_N %in%  paste0("A08-0", 1:9) )) #exclude蘭嶼


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

#---
library(DBI)
library(dbx)
con <- dbxConnect(adapter="sqlite", dbname="D:/R/test/DB/P_BBS.db")
list_Site<-
  dbReadTable(con, "list_Site") %>% 
  select(Site_N = 樣區編號,County =縣市)
#----
M.data.1 <- 
  M.data.1 %>%
  mutate(DATE = paste(Year, Month, Day, sep = "-") %>% as.Date()) %>% 
  mutate(TypeName.1 = case_when(
    TypeName %in% grep("混",.$TypeName , fixed = T, value = T)  ~ "混淆林",
    TypeName %in% "闊葉樹林型" ~ "闊葉林",
    TypeName %in% "竹林"       ~ "竹林",
    TypeName %in% "針葉樹林型" ~ "針葉林",
  )) %>% 
  mutate(TypeName.1 = ifelse(Distance>20, "非森林", TypeName.1)) %>% 
  mutate(TypeName.1 = ordered(TypeName.1, c("闊葉林",
                                            "針葉林",
                                            "混淆林",
                                            "竹林",
                                            "非森林"))) %>% 
  left_join(list_Site, by = "Site_N") %>% 
  mutate(County = gsub("台", "臺", County)) %>% 
  mutate(Region2 = case_when(
    County %in% c("宜蘭縣","基隆市","臺北市","新北市",
                  "桃園市","新竹市","新竹縣","苗栗縣") ~ "North",
    County %in% c("臺中市", "彰化縣","南投縣","南投市")~ "Center",
    County %in% c("雲林縣","嘉義縣","嘉義市", "臺南市")~ "Southwest",
    County %in% c("高雄市", "屏東縣")                  ~ "South",
    County %in% c("花蓮縣")                            ~ "Hualien",
    County %in% c("臺東縣")                            ~ "Taitung"
  )) %>%
  mutate(julian.D = format(DATE, "%j") %>% as.numeric())

M.data.2 <-
  M.data.1 %>%  
  mutate(analysis = "Y") %>% 
  mutate(analysis = ifelse(Month >= 3 & Month <= 6,  
                           analysis,
                           "N")) %>% 
  mutate(analysis = ifelse(TypeName.1 %in% "非森林" | Altitude<50,
                           "N",
                           analysis))%>% 
  mutate(analysis = ifelse(is.na(X_97) & is.na(Y_97),  
                           "N",
                           analysis)) %>%  
  mutate(analysis = ifelse(! Survey %in% 1:2,
                           "N",
                           analysis)) 


write_xlsx(M.data.2, here("./data/clean/for analysis_1523.xlsx"))



