#---- load library

library(data.table)
library(magrittr)
library(readxl)
library(tidyr)
library(purrr)
library(dplyr)
library(writexl)
#library(rgdal)

#-----------------------------

#Sys.time()

#forest4th <- 
#  readOGR(".",
#          "全島森林林型分布圖", 
#          use_iconv = TRUE, encoding = "UTF-8") %>% 
#  spTransform(CRS("+init=epsg:3826"))
#
#Sys.time()
#forest4th@data%<>% as.data.table 


#f.1 <- forest4th@data %>% .[, .I[ TypeName == "闊葉樹林型" |
#                                    TypeName == "竹針混淆林" |
#                                    TypeName == "竹針闊混淆林"|
#                                    TypeName == "竹闊混淆林"|
#                                    # TypeName == "待成林地"|
#                                    TypeName == "針闊葉樹混"|
#                                    TypeName == "針闊葉樹混淆"|
#                                    # TypeName == "陰影"|
#                                    #  TypeName == "裸露地"|
#                                    TypeName == "針葉樹林型"|
#                                    TypeName == "竹林"]] %>% forest4th[.,]


S.all<- list.files("./data/clean/Site/", pattern = "Site_", full.names = T) %>% 
  lapply(., function(x){
    read_xlsx(x, sheet = 1, cell_cols("A:K"), col_types = "text") %>% 
      setDT
  }) %>% 
  do.call(rbind, .) %>% 
  setDT  %>% 
  .[, Year := as.character(Year)]

S.all %>% .[, .N, by = list(Year, Survey, Site_N, Point)] %>% .[ N >1,]
S.all %>% .[, list(X, Y)] %>% unique %>% .[, NO := 1 : nrow(.)] %>% write.csv(., "./data/clean/gis/S_all.csv", row.names = F)


#S.all<- S.all %>% 
#  .[, list(x=X, y=Y, X, Y)] %>% 
#  unique %>% 
#  .[, NO := 1 : nrow(.)] %>% 
#  .[, X := as.numeric(X)] %>% 
#  .[, Y := as.numeric(Y)] 

#S.all.o <- S.all
#coordinates(S.all) <- ~X + Y
#proj4string(S.all) <- CRS("+init=epsg:4326")

#S.all %<>% spTransform(CRS("+init=epsg:3826"))


#library(maptools)

#Sys.time()
#SS <- over(S.all[1:10,], f.1)  
#Sys.time()


library(sf)
library(nngeo)

nc <- st_read("全島森林林型分布圖.shp", crs="+init=epsg:3826")

nc.1 <-
nc %>% 
  filter(TypeName %in% c("闊葉樹林型", "針葉樹林型", "竹林",
                         "竹針混淆林", "竹針闊混淆林", "竹闊混淆林",
                         "針闊葉樹混","針闊葉樹混淆")) 
st_transform(nc.1, 3826)

st.all<- 
  S.all %>% 
  .[, list(x=X, y=Y, X, Y)] %>% 
  unique %>% 
  .[, NO := 1 : nrow(.)] %>% 
  .[, X := as.numeric(X)] %>% 
  .[, Y := as.numeric(Y)] %>% 
  st_as_sf(., coords = c("X", "Y"), crs = 4326) %>% 
  st_transform(., 3826)

plot(st_geometry(st.all))


Sys.time()
st.new<- st_join(st.all, nc.1, st_nn, k = 1)
Sys.time()
st.dis<- st_nn(st.all, nc.1, k = 1, returnDist = TRUE) %>% do.call(cbind,.)
Sys.time()

