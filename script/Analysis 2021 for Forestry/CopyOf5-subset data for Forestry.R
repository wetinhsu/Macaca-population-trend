#樣點資料檢核
#---- load library

library(tidyverse)
library(readxl)
library(writexl)
library(sf)

#------------
M.data <- read_excel("./data/clean/full_combind_Forestrydata_2021_V2.xlsx", sheet = "Data")%>% 
  select(-Macaca_voice, -Habitat ) %>% 
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
  st_as_sf(., coords = c("TWD97_X", "TWD97_Y"), crs = 3826) %>% 
  split(., .$Y_S_S) %>% 
  lapply(., function(x){

    dist <-   
      st_distance(x$geometry)    #算點與點間的距離
    
      colnames(dist) <- x$Point         #加colnames
      
      dist %>% 
      as.data.frame() %>%   #轉回長表格
      setNames(., x$Point) %>% 
      add_column(Base_point = x$Point) %>% 
      reshape2::melt(id.vars = "Base_point", variable.name = "Nearest_point", value.name = "distance")%>%  
      filter(!distance %in% 0) %>% 
        arrange(Base_point)
    }) %>% 
  bind_rows(.id = "Y_S_S") %>% 
  separate("Y_S_S", c("Year", "Survey","Site_N"), "_") %>% 
  mutate(distance = as.numeric(distance)) %>% 
  mutate(Nearest_point = Nearest_point %>% as.character() %>% as.numeric()) %>% 
  mutate(Base_point = as.numeric(Base_point))

#M.abc %>% write_xlsx(.,"D:/待處理工作夾(做完要歸檔)/重複記錄.xlsx")  
#重複記錄用人工卻認吧，依樣區有3個以上的重複記錄就很麻煩，程式規則也寫很麻煩


M.data.1 <-
    M.data %>% 
    
    #exculde 重複記錄
    mutate(Macaca_sur = replace(Macaca_sur,
                            Year %in% 2021 & Survey %in% 1 & Site_N %in% "MA-A02-06" & Point %in% c(1,3),
                            0)) %>% 
    mutate(Macaca_sur = replace(Macaca_sur,
                                Year %in% 2021 & Survey %in% 1 & Site_N %in% "MA-A05-01" & Point %in% c(4,6),
                                0)) %>% 
    mutate(Macaca_sur = replace(Macaca_sur,
                                Year %in% 2021 & Survey %in% 1 & Site_N %in% "MA-F24-10" & Point %in% c(3),
                                0)) %>% 
    mutate(Macaca_sur = replace(Macaca_sur,
                                Year %in% 2021 & Survey %in% 1 & Site_N %in% "MA-H31-15" & Point %in% c(4),
                                0)) %>% 
    mutate(Macaca_sur = replace(Macaca_sur,
                                Year %in% 2021 & Survey %in% 1 & Site_N %in% "MA-H34-01" & Point %in% c(4),
                                0)) %>% 
    mutate(Macaca_sur = replace(Macaca_sur,
                                Year %in% 2021 & Survey %in% 1 & Site_N %in% "MB-E20-04" & Point %in% c(3, 5),
                                0)) %>% 
    mutate(Macaca_sur = replace(Macaca_sur,
                                Year %in% 2021 & Survey %in% 1 & Site_N %in% "MB-F25-05" & Point %in% c(6),
                                0)) %>% 
    mutate(Macaca_sur = replace(Macaca_sur,
                                Year %in% 2021 & Survey %in% 1 & Site_N %in% "MB-H31-14" & Point %in% c(2),
                                0)) %>%
    mutate(Macaca_sur = replace(Macaca_sur,
                                Year %in% 2021 & Survey %in% 2 & Site_N %in% "MA-A05-01" & Point %in% c(2, 4, 6),
                                0)) %>%
    mutate(Macaca_sur = replace(Macaca_sur,
                                Year %in% 2021 & Survey %in% 2 & Site_N %in% "MA-E21-10" & Point %in% c(1, 4),
                                0)) %>%
    mutate(Macaca_sur = replace(Macaca_sur,
                                Year %in% 2021 & Survey %in% 2 & Site_N %in% "MA-H32-02" & Point %in% c(5),
                                0)) %>%
    mutate(Macaca_sur = replace(Macaca_sur,
                                Year %in% 2021 & Survey %in% 2 & Site_N %in% "MA-H34-12" & Point %in% c(2),
                                0)) %>%
    mutate(Macaca_sur = replace(Macaca_sur,
                                Year %in% 2021 & Survey %in% 2 & Site_N %in% "MB-A01-03" & Point %in% c(2),
                                0)) %>%
    mutate(Macaca_sur = replace(Macaca_sur,
                                Year %in% 2021 & Survey %in% 2 & Site_N %in% "MB-G29-08" & Point %in% c(4),
                                0)) #exculde 重複記錄
    
    


View(M.data.1 )



remove.data  <-  #重複記錄的列表
M.data.1%>% 
  anti_join(M.data)




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

# for analysis2021

M.data.1 <- 
M.data.1 %>%
  mutate(TypeName.1 = ordered(TypeName.1,c("闊葉林", "針葉林","混淆林","竹林","非森林"))) %>% 
  mutate(julian.D = ISOdatetime(Year, Month, Day, Hour, Minute, sec = 0) %>%
           as.POSIXlt(format = "%yyyy%mm%dd") %>%
           format(., "%j") %>% as.numeric())
   
M.data.2 <-
  M.data.1 %>% 
  filter(analysis == "Y") %>%
  filter(Month >= 3 & Month <= 6) %>%   #刪除調查季(包含緩衝期)以外的資料
  mutate(analysis = ifelse(TypeName.1 %in% "非森林" | Altitude<50, "N", analysis)) 


M.data.2 %>% filter(analysis %in% "Y") %>%  #看一下誰被刪掉
  anti_join(M.data.1,.) %>% View



M.data.2 %>%  
  group_by(Year, Survey, Site_N, Point) %>% 
  summarise(N = n()) %>% 
  filter(N >1)

NOTE <- data.frame(說明 = "本資料集為已有刪減，僅留下可納入分析的資料。")



write_xlsx( list('NOTE' = NOTE, 'Data' = M.data.2), "./data/clean/for analysis Forestrydata_2021_V1.xlsx")


