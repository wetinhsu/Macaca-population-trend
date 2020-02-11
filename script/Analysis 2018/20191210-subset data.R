#---- load library

library(data.table)
library(magrittr)
library(readxl)
library(tidyr)
library(purrr)
library(dplyr)
library(writexl)
library(rgdal)
library(rgeos)
#-----------------------------
M.data <- read_excel("./data/clean/full_combind_data_V1.xlsx") %>% 
  setDT


M.data %<>%  
  dcast.data.table(.,Year + Site_N ~ Survey, value.var = "Macaca_sur") %>%  
  #計算第1旅次及第2旅次調查的樣點數
  .[`1` %in% 0,] %>%  #找出有第2旅次沒第1旅次的樣區
  .[M.data, on = c("Year", "Site_N")] %>%  
  .[!is.na(`1`), Survey := 1] %>%  #將第1次調查的旅次改回1
  .[,`1` := NULL]%>%
  .[,`2` := NULL]




M.0<- M.data %>% .[!is.na(Macaca_sur),] %>% 
  .[Macaca_sur %in% 1,] 

M.2<- M.0 %>% 
  .[, .N, list(Year, Survey, Site_N)] %>% 
  .[N>1,] %>% 
  .[order(Year,Survey, Site_N), ]


M.abc <- M.2 %>%
  left_join(M.0) %>% setDT %>% 
  .[, Y_S_S := paste0(Year, "_", Survey, "_",Site_N)] %>% 
  split(., .$Y_S_S) %>% 
  lapply(., function(x){
    rownames(x) <- paste0(x$Site_N,"-",x$Point)
    coordinates(x) <- ~X + Y
    proj4string(x) <- CRS("+init=epsg:4326")
    x %<>% spTransform(CRS("+init=epsg:3826"))
    gDistance(x, x,byid=TRUE) %>% apply(.,2,function(k){
      paste0(
        names(k[k>0])[which.min(k[k>0])]," /// ",
        round(min(k[k>0]),3)
      )} ) %>%
      data.frame (Base_point=names(.),value=.) %>%  #把row的names塞成column
      data.table(.)  #除去row的names
  }) %>%
  rbindlist(., idcol=TRUE) %>% 
  separate("value", c("Nearest_point","Distance"), "///")  %>% 
  separate(".id", c("Year", "Survey","Site_N"), "_") %>% 
  .[, c("Site_N") := NULL] %>% 
  .[, Distance := as.numeric(Distance)]  

(remove.data <- 
  M.abc %>%  
  .[  Distance<300,] %>% #猴群距離<300m者的第2筆，可能為重複記錄，故刪除猴群記錄
  .[, Site_N := substr(Base_point,1,6)] %>%
  .[, Point := substr(Base_point,8,8)] %>% 
  .[, Point:= as.numeric(Point)] %>% 
  .[, .(Point=max(Point)), by = list(Year, Survey, Site_N)] %>% 
  setDT)



output.1<- M.0 %>% 
  .[, .N, list(Year, Survey, Site_N)] %>% 
  .[, Y_S := paste0(Year,"-", Survey)] %>% 
  dcast(., Site_N ~ Y_S, value.var ="N") %>% 
  setDT #歷年來各樣區內的猴群數

write_xlsx(list(
  "group.Site" = output.1,
  "groupDistance" = M.abc),
  "./result/20191210-cheak distacne_V1.xlsx")


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
    M.data.1 %>%  .[julian.D > 60 & julian.D <= 180, ] 

M.data.1 %>%  .[!(julian.D > 60 & julian.D <= 180), ]  %>% 
  dcast(Year + Survey ~ julian.D, value.var = "Point")
  


M.data.2 %>%  .[, .N, by = list(Year, Survey, Site_N, Point)] %>% .[ N >1,]


write_xlsx(M.data.2, "./data/clean/for analysis_V1.xlsx")
