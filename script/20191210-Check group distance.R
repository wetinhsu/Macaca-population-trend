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

M.2<- M.data %>% .[!is.na(Macaca_sur),] %>% 
  .[Macaca_sur %in% 1,]%>% 
  .[, .N, list(Year, Survey, Site_N)] %>% 
  .[N>1,] %>% 
  .[order(Year,Survey, Site_N), ]



M.0<- M.data %>% .[!is.na(Macaca_sur),] %>% 
  .[Macaca_sur %in% 1,]

M.m <- M.2 %>%
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
  do.call(rbind, .)%>% 
  separate("value", c("Nearest_point","Distance"), "///") 

