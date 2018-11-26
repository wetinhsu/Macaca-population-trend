#---- load library
library(here)
library(data.table)
library(magrittr)
library(readxl)
library(tidyr)
library(purrr)
library(dplyr)
library(writexl)

#---- import data
# 2015
S15 <- 
  read_xlsx("data/raw/Macaca_2015_2017_analysis.xlsx",
            sheet = 2) %>% 
  setDT %>% 
  .[Year %in% 2015, 
    list(Site_N, Point, TypeName_O, Distance_O,
         X_new, Y_new,
         Year, Survey)] %>% 
  dcast(Site_N + Point + TypeName_O + Distance_O + X_new + Y_new~
          Year + Survey,
        fill = NA, fun = length)
# 2016
S16 <- 
  read_xlsx("data/raw/Macaca_2015_2017_analysis.xlsx",
            sheet = 2) %>% 
  setDT %>% 
  .[Year %in% 2016, 
    list(Site_N, Point, TypeName_O,
         X_new, Y_new,
         Year, Survey)] %>% 
  dcast(Site_N + Point + TypeName_O + X_new + Y_new~
          Year + Survey,
        fill = NA, fun = length)

# 2017
S17 <- 
  read_xlsx("data/raw/2017獼猴調查(樣區整理)_分析用.xlsx",
            sheet = 2) %>% 
  setDT %>% 
  .[, list(`樣點編號`,
           X_2017 = WGS84_X__1,
           Y_2017 = WGS84_Y__1)] %>% 
  separate("樣點編號", c("Site_a", "Site_b","Point"), "-") %>% 
  .[, Site_N := paste(Site_a, Site_b, sep = "-")] %>% 
  .[, c("Site_a", "Site_b") := NULL] %>% 
  .[, Point := as.numeric(Point)]
S17.survey.condition <- 
  read_xlsx("data/raw/2017獼猴調查(樣區整理)_分析用.xlsx",
            sheet = 1) %>% 
  setDT %>% 
  .[, list(`樣區\r\n編號`, 第一旅次, 第二旅次)] %>% 
  setnames(c("Site_N" ,"2017_1", "2017_2")) %>% 
  .[, list(`2017_1` = ifelse(is.na(`2017_1`), 0, 1),
           `2017_2` = ifelse(is.na(`2017_2`), 0, 1)),
    by = Site_N] %>% 
  unique
  
S17 %<>% S17.survey.condition[., on = "Site_N"]  

# 2018
S18 <- 
  read_xlsx("data/raw/2018獼猴調查(樣區整理)_分析用.xlsx",
            sheet = 2,
            col_types = "text") %>% 
  setDT %>% 
  .[, list(`樣點編號`,
           X_2018 = as.numeric(WGS84_X__1),
           Y_2018 = as.numeric(WGS84_Y__1))] %>% 
  separate("樣點編號", c("Site_a", "Site_b","Point"), "-") %>% 
  .[, Site_N := paste(Site_a, Site_b, sep = "-")] %>% 
  .[, c("Site_a", "Site_b") := NULL] %>% 
  .[, Point := as.numeric(Point)]
S18.survey.condition <- 
  read_xlsx("data/raw/2018獼猴調查(樣區整理)_分析用.xlsx",
            sheet = 1) %>% 
  setDT %>% 
  .[, list(`樣區\r\n編號`, 第一旅次, 第二旅次)] %>% 
  setnames(c("Site_N" ,"2018_1", "2018_2")) %>% 
  .[, list(`2018_1` = ifelse(is.na(`2018_1`), 0, 1),
           `2018_2` = ifelse(is.na(`2018_2`), 0, 1)),
    by = Site_N]

S18 %<>% S18.survey.condition[., on = "Site_N"]  

# left_join S17, S18 with S1516
site_info <- 
  reduce(list(S15, S16, S17, S18), 
         full_join, 
         by = c("Site_N", "Point")) %>% 
  setDT

# Deal with coordinate
site_info.F <- 
  site_info %>%  
  .[is.na(TypeName_O.x), TypeName_O.x := TypeName_O.y] %>% 
  .[, TypeName_O.y := NULL] %>% 
  .[is.na(X_new.x), X_new.x := X_new.y] %>% 
  .[is.na(Y_new.x), Y_new.x := Y_new.y] %>% 
  .[, c("X_new.y", "Y_new.y") := NULL] %>% 
  .[is.na(X_new.x), X_new.x := X_2017] %>% 
  .[is.na(Y_new.x), Y_new.x := Y_2017] %>%
  .[, c("X_2017", "Y_2017") := NULL] %>% 
  .[is.na(X_new.x), X_new.x := X_2018] %>% 
  .[is.na(Y_new.x), Y_new.x := Y_2018] %>%
  .[, c("X_2018", "Y_2018") := NULL]

# Environment factor
##read shapefile for value
library(rgdal)
library(rgeos)
no.Fo <- 
  site_info.F[is.na(TypeName_O.x)]
coordinates(no.Fo) <- ~X_new.x + Y_new.x
proj4string(no.Fo) <- CRS("+init=epsg:4326")
no.Fo %<>% spTransform(CRS("+init=epsg:3826"))

forest4th <- 
  readOGR("data/layer", "Forest_4th", 
          use_iconv = TRUE, encoding = "UTF-8") %>% 
  spTransform(CRS("+init=epsg:3826"))

# Distance <- 
#   gDistance(no.Fo, forest4th, byid = T) %>% 
#   as.data.frame
## For each point, find name of nearest polygon
# n <- nrow(no.Fo)
# F.type <- character(n)
# min.D <- character(n)
# for (i in seq_along(F.type)) {
#   forest.type <- forest4th@data$TypeName
#   which_min <- which.min(Dist.Fo[,i])
#   min.D[i] <- min(Dist.Fo[i])
#   F.type[i] <- paste0(forest.type[which_min])
# }
# no.Fo.dat <- 
#   data.frame(Site_N = no.Fo@data$Site_N,
#              Point = no.Fo@data$Point,
#              TypeName = F.type,
#              Distance = as.numeric(min.D))


#### test 2018.11.26 ####
library(parallel)

cl <- makeCluster(6)
clusterExport(cl, c("no.Fo",
                    "forest4th"))
clusterEvalQ(cl, 
             c(library(data.table),
               library(rgdal),
               library(rgeos),
               library(magrittr))) %>% 
  invisible

no.Fo.dat <- 
  parLapply(cl, 1:nrow(no.Fo), 
         function(x)
           data.table(Site_N = no.Fo@data$Site_N[x],
                      Point = no.Fo@data$Point[x],
                      Distance = as.vector(gDistance(no.Fo[x,], forest4th, byid = T)),
                      TypeName = forest4th@data$TypeName) %>% 
           .[Distance == min(Distance)]) %>% 
  do.call(rbind, .) %>% 
  unique

stopCluster(cl)


site_info.final <- 
  left_join(site_info.F, no.Fo.dat, by = c("Site_N", "Point")) %>% 
  setDT %>% 
  .[is.na(TypeName_O.x), TypeName_O.x := TypeName] %>% 
  .[is.na(Distance_O), Distance_O := Distance] %>% 
  .[, c("TypeName", "Distance") := NULL] %>% 
  .[Site_N == "A29-27" & Point == "7", TypeName_O.x := "竹闊混淆林"] %>% 
  unique

write_xlsx(site_info.final, 
           "data/clean/point_Forest_1518.xlsx")
