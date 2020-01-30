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
M.data <- read_excel("./data/clean/full_combind_data.xlsx") %>% 
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
  "./result/20191210-cheak distacne.xlsx")


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
                    "A08-05", "A08-06", "A08-07", "A08-08", "A08-09")),] %>% 
  .[Macaca_sur %in% 1 & Macaca_dist %in% "C", Mcaca_sur := 0]   #exculde >100m



M.data.1 %>%  .[, .N, by = list(Year, Survey, Site_N, Point)] %>% .[ N >1,]


write_xlsx(M.data.1, "./data/clean/for analysis.xlsx")
