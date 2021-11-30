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
  filter(analysis %in% "Y") %>% 
  filter(Macaca_sur %in% 1)  #猴群

M.2<- M.0 %>%  #找出同一旅次同一樣區有猴群大於2者
  filter(analysis %in% "Y") %>% 
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
                              Year %in% 2020 & Survey %in% 1 & Site_N %in% "MA-A05-01" & Point %in% 4,
                              0)) %>% 
  mutate(Macaca_sur = replace(Macaca_sur,
                              Year %in% 2020 & Survey %in% 1 & Site_N %in% "MA-E21-11" & Point %in% 5,
                              0)) %>% 
  mutate(Macaca_sur = replace(Macaca_sur,
                              Year %in% 2020 & Survey %in% 1 & Site_N %in% "MA-F25-01" & Point %in% 4,
                              0)) %>% 
  mutate(Macaca_sur = replace(Macaca_sur,
                              Year %in% 2020 & Survey %in% 1 & Site_N %in% "MA-G28-01" & Point %in% 3,
                              0)) %>% 
  mutate(Macaca_sur = replace(Macaca_sur,
                              Year %in% 2020 & Survey %in% 1 & Site_N %in% "MA-G28-06" & Point %in% 5,
                              0)) %>% 
  mutate(Macaca_sur = replace(Macaca_sur,
                              Year %in% 2020 & Survey %in% 1 & Site_N %in% "MA-H31-04" & Point %in% c(2, 4),
                              0)) %>% 
  mutate(Macaca_sur = replace(Macaca_sur,
                              Year %in% 2020 & Survey %in% 1 & Site_N %in% "MA-H32-06" & Point %in% c(2, 4),
                              0)) %>% 
  mutate(Macaca_sur = replace(Macaca_sur,
                              Year %in% 2020 & Survey %in% 1 & Site_N %in% "MA-H33-01" & Point %in% 3,
                              0)) %>% 
  mutate(Macaca_sur = replace(Macaca_sur,
                              Year %in% 2020 & Survey %in% 1 & Site_N %in% "MA-H34-01" & Point %in% 5,
                              0)) %>%
  mutate(Macaca_sur = replace(Macaca_sur,
                              Year %in% 2020 & Survey %in% 1 & Site_N %in% "MA-H34-04" & Point %in% 2,
                              0)) %>%
  mutate(Macaca_sur = replace(Macaca_sur,
                              Year %in% 2020 & Survey %in% 1 & Site_N %in% "MA-H34-05" & Point %in% 2,
                              0)) %>%
  mutate(Macaca_sur = replace(Macaca_sur,
                              Year %in% 2020 & Survey %in% 1 & Site_N %in% "MA-H34-11" & Point %in% 4,
                              0)) %>%
  mutate(Macaca_sur = replace(Macaca_sur,
                              Year %in% 2020 & Survey %in% 2 & Site_N %in% "MA-E21-11" & Point %in% c(3,5),
                              0)) %>%
  mutate(Macaca_sur = replace(Macaca_sur,
                              Year %in% 2020 & Survey %in% 2 & Site_N %in% "MA-F25-11" & Point %in% 3,
                              0)) %>%
  mutate(Macaca_sur = replace(Macaca_sur,
                              Year %in% 2020 & Survey %in% 2 & Site_N %in% "MA-G29-03" & Point %in% 2,
                              0)) %>%
  mutate(Macaca_sur = replace(Macaca_sur,
                              Year %in% 2020 & Survey %in% 2 & Site_N %in% "MA-H31-04" & Point %in% 2,
                              0)) %>%
  mutate(Macaca_sur = replace(Macaca_sur,
                              Year %in% 2020 & Survey %in% 2 & Site_N %in% "MA-H31-09" & Point %in% 2,
                              0)) %>%
  mutate(Macaca_sur = replace(Macaca_sur,
                              Year %in% 2020 & Survey %in% 2 & Site_N %in% "MA-H31-13" & Point %in% 2,
                              0)) %>%
  mutate(Macaca_sur = replace(Macaca_sur,
                              Year %in% 2020 & Survey %in% 2 & Site_N %in% "MA-H34-01" & Point %in% 5,
                              0)) %>%
  mutate(Macaca_sur = replace(Macaca_sur,
                              Year %in% 2020 & Survey %in% 2 & Site_N %in% "MB-A01-06" & Point %in% 3,
                              0)) %>%
  mutate(Macaca_sur = replace(Macaca_sur,
                              Year %in% 2020 & Survey %in% 2 & Site_N %in% "MB-B06-10" & Point %in% 6,
                              0)) %>%  #exculde 重複記錄

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
  mutate(analysis = ifelse(analysis == "Y" &( Month < 3 | Month > 6), 
                           "Y(month)", analysis)) %>%   #刪除調查季(包含緩衝期)以外的資料
  mutate(analysis = ifelse(analysis == "Y" &  Altitude<50, 
                           "Y(50m)", analysis)) %>%  
  mutate(analysis = ifelse(analysis == "Y" &  TypeName.1 %in% "非森林", 
                           "Y(NotForest)", analysis)) 

point_summary <- 
M.data.2 %>% 
  summarise(
    '1 收到' = n(),
    '2-1 6min' = str_subset(analysis, "6min") %>% length,
    '2-2 Toolate' = str_subset(analysis, "Toolate") %>% length,
    '2-3 7day' = str_subset(analysis, "7day") %>% length,
    '2-4 locate' = str_subset(analysis, "locate") %>% length,
    'end of 2' = str_subset(analysis, "locate|6min|Toolate|7day", negate = T) %>% length,
    '3 不在3~6月內' = str_subset(analysis, "month") %>% length,
    '4 低於50m' = str_subset(analysis, "50m") %>% length,
    '5 非森林' = str_subset(analysis, "NotForest") %>% length,
    '6 分析用' = str_subset(analysis, "^Y$") %>% length
  ) %>% 
  
  reshape2::melt(variable.name = "項目",
                 value.name = "樣點次")



M.data.2 %>%  
  group_by(Year, Survey, Site_N, Point) %>% 
  summarise(N = n()) %>% 
  filter(N >1)

NOTE <- data.frame(
  說明 = c(
    "6min, Toolate, 7day, locate = 不符合調查規範",
    "month = 符合調查規範，但月份不在3~6月",
    "50m = 符合調查規範述，月份在3~6月，但海拔<50m",
    "NotForest = 符合調查規範述，月份在3~6月，海拔>=50m，但非森林",
    "Y = 符合調查規範述，月份在3~6月，海拔>=50m，只有森林"
         ))



write_xlsx( list('NOTE' = NOTE,
                 'Point_Summary' = point_summary,
                 'Data' = M.data.2), 
            paste0("./data/clean/Forestry/for analysis/for analysis Forestrydata_2021_V1.xlsx"))


