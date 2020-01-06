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

#-----------------------------------

M.data <- read_excel("./data/clean/for analysis.xlsx") %>% 
  setDT

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


output.1<- M.0 %>% 
  .[, .N, list(Year, Survey, Site_N)] %>% 
  .[, Y_S := paste0(Year,"-", Survey)] %>% 
  dcast(., Site_N ~ Y_S, value.var ="N") %>% 
  setDT 


M.abc %>% .[Distance<300,]  #猴群距離<300m者的第2筆，可能為重複記錄，故刪除猴群記錄



#------------------------------------------------------------

M.3<- M.0 %>% 
  .[Macaca_dist %in% c("A", "B"),]%>% 
  .[, .N, list(Year, Survey, Site_N)] %>% 
  .[N>1,] %>% 
  .[order(Year,Survey, Site_N), ]


M.ab <- M.0 %>%
  .[Macaca_dist %in% c("A", "B"),] %>% 
  left_join(M.3, .) %>% setDT %>% 
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
  separate("value", c("Nearest_point","Distance"), "///") %>% 
  separate(".id", c("Year", "Survey","Site_N"), "_") %>% 
  .[, c("Site_N") := NULL] %>% 
  .[, Distance := as.numeric(Distance)]


