#樣點資料檢核
#---- load library

library(tidyverse)
library(readxl)
library(writexl)
library(sf)

#------------
M.data <- read_excel("./data/clean/full_combind_Forestrydata_V1.xlsx") 




M.0<- M.data %>%
  filter(Macaca_sur %in% 1)  #猴群

M.2<- M.0 %>% 
  group_by(Year, Survey, Site_N) %>% 
  summarise(N = n()) %>% 
  filter(N>1) %>% 
  arrange(Year,Survey, Site_N)
  


M.abc <- M.2 %>%
  left_join(M.0) %>% 
  mutate(Y_S_S = paste0(Year, "_", Survey, "_",Site_N)) %>% 
  st_as_sf(., coords = c("X", "Y"), crs = 3826) %>% 
  split(., .$Y_S_S) %>% 
  lapply(., function(x){

    dist =  st_distance(x$geometry) 
    
    colnames(dist) = x$Point
    
     dist %>% 
      as.data.frame() %>% 
      setNames(., x$Point) %>% 
      add_column(Base_point = x$Point) %>% 
      reshape2::melt(id.vars = "Base_point", variable.name = "Nearest_point", value.name = "Distance")
    
    }) %>% 
  bind_rows(.id = "Y_S_S")



df %>%
  group_by(gr) %>%
  mutate(
    dist = st_distance(geometry)
  )

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


