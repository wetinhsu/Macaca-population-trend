#---- load library

library(data.table)
library(magrittr)
library(readxl)
library(tidyr)
library(purrr)
library(dplyr)
library(writexl)
library(rgdal)

#-----------------------------
M.all <- read_excel("./data/clean/Macaca/Macaca_1519_v1.xlsx") %>% 
  setDT


S.all<- list.files("./data/clean/Site/", pattern = "Site_", full.names = T) %>% 
    lapply(., function(x){
      read_xlsx(x, sheet = 1, cell_cols("A:K")) %>% 
        setDT
    }) %>% 
    do.call(rbind, .) %>% 
    setDT

S.all %>% .[, .N, by = list(Year, Survey, Site_N, Point)] %>% .[ N >1,]
S.all %>% .[, list(X, Y)] %>% unique %>% .[, NO := 1 : nrow(.)] %>% write.csv(., "./data/clean/gis/S_all.csv", row.names = F)

M.data<- M.all %>% 
  full_join(S.all, by = c("Year", "Site_N", "Point", "Survey")) %>% 
  setDT %>% 
  .[order(Year,Site_N),] %>% 
  .[, X := as.numeric(X)] %>% 
  .[, Y := as.numeric(Y)]

M.data %>% .[, .N, by = list(Year, Survey, Site_N, Point)] %>% .[ N >1,]

#------------

Forest<- read.csv("./data/clean/gis/S_all_TypeName.csv") %>% 
  setDT %>% 
  .[, c("NO", "join_Function", "join_FunctionTy", "join_Area_ha")  := NULL] %>% 
  setnames(.,c("X", "Y", "TypeName", "Distance"))

Altitude<- read.csv("./data/clean/gis/S_all_Altitude.csv") %>% 
  setDT %>% 
  setnames(.,c("Altitude", "X", "Y"))

M.data %<>%
  left_join(Forest, by = c("X", "Y")) %>% 
  left_join(Altitude, by = c("X", "Y")) %>% 
  setDT 
write_xlsx(M.data, "./data/clean/full_combind_data.xlsx")







#---- Fill na forest type
library(rgdal)
library(rgeos)
library(magrittr)
library(data.table)

setwd("C:/Users/wetin/Desktop/R/第四次森林資源調查全島森林林型分布圖")

Sys.time()

forest4th <- 
  readOGR(".",
          "全島森林林型分布圖", 
          use_iconv = TRUE, encoding = "UTF-8") %>% 
  spTransform(CRS("+init=epsg:3826"))

Sys.time()

#---------------------------

M.data.o <- M.data
coordinates(M.data) <- ~X + Y
proj4string(M.data) <- CRS("+init=epsg:4326")
M.data %<>% spTransform(CRS("+init=epsg:3826"))

forest4th@data%<>% as.data.table 

f.1 <- forest4th@data %>% .[, .I[ TypeName == "闊葉樹林型" |
                               TypeName == "竹針混淆林" |
                               TypeName == "竹針闊混淆林"|
                               TypeName == "竹闊混淆林"|
                             # TypeName == "待成林地"|
                               TypeName == "針闊葉樹混"|
                               TypeName == "針闊葉樹混淆"|
                             # TypeName == "陰影"|
                            #  TypeName == "裸露地"|
                               TypeName == "針葉樹林型"|
                               TypeName == "竹林"]] %>% forest4th[.,]


m = gDistance(M.data, f.1,byid=TRUE)
dim(m)
col = apply(m, 2, function(x) which(x==min(x)))

#----------------------------------------
M.1<- M.data %>% .[!is.na(Macaca_sur),]
M.0<- M.data %>% .[!is.na(Macaca_sur),]
coordinates(M.1) <- ~X + Y
proj4string(M.1) <- CRS("+init=epsg:4326")
M.1 %<>% spTransform(CRS("+init=epsg:3826"))

#---------------------------------------
M.0 %>% 
  .[, Y_S := paste0(Year, "_", Survey)] %>% 
  split(., .$Y_S) %>% 
  lapply(., function(x){
    coordinates(x) <- ~X + Y
    proj4string(x) <- CRS("+init=epsg:4326")
    x %<>% spTransform(CRS("+init=epsg:3826"))
    gDistance(x, x,byid=TRUE) %>% apply(.,2,function(k) min(k[k>0]))
  })
