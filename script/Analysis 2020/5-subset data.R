#load library---- 

library(data.table)
library(tidyverse)
library(readxl)
library(tidyr)
library(writexl)
library(sf)

#-----------------------------
M.data <- read_excel("./data/clean/full_combind_data_V2.xlsx")  %>% 
  mutate(Macaca_sur = ifelse(Macaca_dist %in% "C" , NA, Macaca_sur)) 


#確認旅次---
M.data <- 
M.data %>%  
  reshape2::dcast(Year + Site_N ~ Survey, length, value.var = "Macaca_sur") %>% 
  #計算第1旅次及第2旅次調查的樣點數
  filter(`1` %in% 0 ) %>%  #找出有第2旅次沒第1旅次的樣區
  left_join(M.data, ., by = c("Year", "Site_N"))%>%
  mutate( Survey = ifelse(!is.na(`1`),1, Survey)) %>% #將第1次調查的旅次改回1
  select(-`1`, -`2`) 




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
  st_as_sf(., coords = c("X", "Y"), crs = 4326) %>% 
  st_transform(., 3826) %>% 
  
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
  filter(!(Site_N %in% c("A08-01", "A08-02", "A08-03", "A08-04",   #exclude蘭嶼
                    "A08-05", "A08-06", "A08-07", "A08-08", "A08-09")))








output.1<- M.0 %>%  #歷年來各樣區內的猴群數
  group_by(Year, Survey, Site_N) %>% 
  summarise(N = n()) %>% 
  mutate(Y_S = paste0(Year,"-", Survey)) %>% 
  reshape2::dcast(Site_N ~ Y_S, value.var ="N")


write_xlsx(list(
  "group.Site" = output.1,
  "groupDistance" = M.abc),
  "./result_2019/20191210-cheak distacne_V1.xlsx")


M.data.1 <- 
  M.data %>%
  setDT %>% 
  .[Year %in% 2015 & Survey %in% 2 & Site_N %in% "A29-17" & Point %in% 7,
    Macaca_sur := NA] %>% 
  .[Year %in% 2015 & Survey %in% 2 & Site_N %in% "A33-28" & Point %in% 7,
    Macaca_sur := NA] %>% 
  .[Year %in% 2016 & Survey %in% 1 & Site_N %in% "B33-01" & Point %in% 4,
    Macaca_sur := NA] %>% 
  .[Year %in% 2017 & Survey %in% 1 & Site_N %in% "B38-07" & Point %in% 7,
    Macaca_sur := NA] %>% 
  .[Year %in% 2018 & Survey %in% 1 & Site_N %in% "A35-15" & Point %in% 5,
    Macaca_sur := NA] %>% 
  .[Year %in% 2018 & Survey %in% 2 & Site_N %in% "A28-10" & Point %in% 6,
    Macaca_sur := NA] %>% 
  .[Year %in% 2019 & Survey %in% 1 & Site_N %in% "B14-02" & Point %in% 6,
    Macaca_sur := NA] %>% 
  .[Year %in% 2019 & Survey %in% 1 & Site_N %in% "B38-08" & Point %in% 5,
    Macaca_sur := NA] %>% 
  .[Year %in% 2019 & Survey %in% 2 & Site_N %in% "A20-02" & Point %in% 3,
    Macaca_sur := NA] %>% 
  .[Year %in% 2019 & Survey %in% 2 & Site_N %in% "A33-32" & Point %in% 6,
    Macaca_sur := NA]  %>% #exculde 重複記錄
  .[!(Site_N %like% "K"),] %>%   #exculde kiman
  .[!(Site_N %in% c("A08-01", "A08-02", "A08-03", "A08-04",   #exclude蘭嶼
                    "A08-05", "A08-06", "A08-07", "A08-08", "A08-09")),] 

# for analysis2015-2019

M.data.1 %<>%
  setDT %>% 
  .[Macaca_dist %in% "C", Macaca_sur := NA] %>% #exculde >100m
  .[, DATE := as.IDate(paste(Year, Month, Day, sep = "/"))] %>% 
  .[TypeName %like% "混", TypeName.n := "混淆林"] %>% 
  .[TypeName %like% "竹林", TypeName.n := "竹林"] %>% 
  .[TypeName %like% "闊葉樹林型", TypeName.n := "闊葉林"] %>% 
  .[TypeName %like% "針葉樹林型", TypeName.n := "針葉林"] %>% 
  .[, TypeName.1 := ifelse(Distance>20, "非森林", TypeName.n)] %>% 
  .[, TypeName.1 := ordered(TypeName.1,c("闊葉林", "針葉林","混淆林","竹林","非森林"))] %>% 
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
                          "台東縣","臺東縣"))] %>% 
  
  .[County %in% list("宜蘭縣","基隆市","台北市","臺北市",
                     "新北市","台北縣","臺北縣",
                     "桃園縣","桃園市","新竹市",
                     "新竹縣","苗栗縣"), Region := "North"] %>%
  .[County %in% list("台中市","臺中市",
                     "台中縣","臺中縣",
                     "彰化縣","南投縣","南投市",
                     "雲林縣","嘉義縣","嘉義市"), Region := "Center"] %>%
  .[County %in% list("台南市","臺南市",
                     "台南縣","臺南縣",
                     "高雄縣","高雄市",
                     "屏東縣"), Region := "South"]%>%
  .[County %in% list("花蓮縣",
                     "台東縣","臺東縣"), Region := "East"] %>% 
  .[, julian.D := yday(DATE)] %>% 
  .[, Altitude_c := substr(Site_N,1,1)] %>% setDT %>% 
  
  .[County %in% list("宜蘭縣","基隆市","台北市","臺北市",
                     "新北市","台北縣","臺北縣",
                     "桃園縣","桃園市","新竹市",
                     "新竹縣","苗栗縣"), Region2 := "North"] %>% 
  .[County %in% list("台中市","臺中市",
                     "台中縣","臺中縣",
                     "彰化縣","南投縣","南投市"), Region2 := "Center1"] %>% 
  .[County %in% list("雲林縣","嘉義縣","嘉義市",
                     "台南市","臺南市",
                     "台南縣","臺南縣"), Region2 := "Center2"] %>%
  .[County %in% list("高雄縣","高雄市",
                     "屏東縣"), Region2 := "South"]%>%
  .[County %in% list("花蓮縣"), Region2 := "East1"] %>%
  .[County %in% list("台東縣","臺東縣"), Region2 := "East2"]  


M.data.2 <-
  M.data.1 %>%  
  .[Month >= 3 & Month <= 6, ] %>%   #刪除調查季(包含緩衝期)以外的資料
  .[, analysis := "N"] %>% 
  .[Altitude >= 50, analysis := "Y"] #刪除海拔未滿50公尺的樣點



(M.data.1 %>%  .[Month < 3 | Month > 6, ]  %>%  #看一下誰被刪掉
    .[, Macaca_sur := as.numeric(Macaca_sur)] %>% 
    .[, .(N.point = .N, m = sum(Macaca_sur, na.rm=T)), by = list( Year, Survey, julian.D)] %>% 
    .[, N.point := as.numeric(N.point)] %>% 
    melt(id.vars = c("Year", "Survey", "julian.D")) %>% 
    dcast(Year + Survey+ variable  ~ julian.D , value.var = c("value")))



#確認一下每一個樣點的資料筆數
M.data.2 %>%  .[, .N, by = list(Year, Survey, Site_N, Point)] %>% .[ N >1,]


write_xlsx(M.data.2, "./data/clean/for analysis_V1.xlsx")

