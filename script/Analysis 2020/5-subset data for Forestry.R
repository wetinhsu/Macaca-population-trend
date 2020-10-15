#樣點資料檢核
#---- load library

library(tidyverse)
library(readxl)
library(writexl)
library(sf)

#------------
M.data <- read_excel("./data/clean/full_combind_Forestrydata_V1.xlsx")%>% 
  mutate(Macaca_sur = ifelse(Macaca_sur %in% "1" , 0,    #孤猴改成0，猴群改成1
                             ifelse(Macaca_sur %in% "2" , 1, Macaca_sur))) %>% 
  mutate(Macaca_sur = ifelse(Macaca_dist %in% "C" , 0, Macaca_sur)) 




M.0<- M.data %>%
  filter(Macaca_sur %in% 1)  #猴群

M.2<- M.0 %>%  #找出同一旅次同一樣區有猴群大於2者
  group_by(Year, Survey, Site_N) %>% 
  summarise(N = n()) %>% 
  filter(N>1) 
  


M.abc <- M.2 %>%
  left_join(M.0) %>%   #幫M.2這樣區樣點加回座標等資訊
  mutate(Y_S_S = paste0(Year, "_", Survey, "_",Site_N)) %>% 
  st_as_sf(., coords = c("X", "Y"), crs = 3826) %>% 
  split(., .$Y_S_S) %>% 
  lapply(., function(x){

    dist <-   
      st_distance(x$geometry)    #算點與點間的距離
    
      colnames(dist) <- x$Point         #加colnames
      
      dist %>% 
      as.data.frame() %>%   #轉回長表格
      setNames(., x$Point) %>% 
      add_column(Base_point = x$Point) %>% 
      reshape2::melt(id.vars = "Base_point", variable.name = "Nearest_point", value.name = "distance")
    
    }) %>% 
  bind_rows(.id = "Y_S_S") %>% 
  separate("Y_S_S", c("Year", "Survey","Site_N"), "_") %>% 
  mutate(distance = as.numeric(distance)) %>% 
  mutate(Nearest_point = Nearest_point %>% as.character() %>% as.numeric()) %>% 
  mutate(Base_point = as.numeric(Base_point)) 


(remove.data <- 
    M.abc %>%  
    filter(distance<300) %>% #猴群距離<300m者的第2筆，可能為重複記錄，故刪除猴群記錄
    group_by(Year, Survey, Site_N) %>% 
    summarise(Point=max(Nearest_point)) #編號較大的，列為被重複記錄
  )

remove.data %>% 
  left_join(M.0)



M.data.1 <- 
remove.data %>% 
  mutate(re = "rep") %>%  #增加輔助欄
  full_join(M.data) %>% 
  mutate(Macaca_sur = ifelse(re %in% "rep", 0, Macaca_sur)) %>% 
  select(-re) #刪掉輔助欄

#M.abc %>% filter(!distance==0) %>% write_xlsx(.,"D:/待處理工作夾(做完要歸檔)/重複記錄.xlsx")

# for analysis2020

M.data.1 <- 
M.data.1 %>%
  mutate(TypeName.1 = ordered(TypeName.1,c("闊葉林", "針葉林","混淆林","竹林","非森林"))) %>% 
  mutate(julian.D = Date %>% as.POSIXlt(format = "%yyyy%mm%dd") %>% format(., "%j") %>% as.numeric())
   
M.data.2 <-
  M.data.1 %>%  
  filter(Month >= 3 & Month <= 6) %>%   #刪除調查季(包含緩衝期)以外的資料
  mutate(analysis = ifelse(TypeName.1 %in% "非森林" | Altitude<50, "N", analysis)) 


M.data.2 %>% filter(analysis %in% "Y") %>%  #看一下誰被刪掉
  anti_join(M.data.1,.) %>% View



M.data.2 %>%  
  group_by(Year, Survey, Site_N, Point) %>% 
  summarise(N = n()) %>% 
  filter(N >1)


write_xlsx(M.data.2, "./data/clean/for analysis Forestrydata_V1.xlsx")


