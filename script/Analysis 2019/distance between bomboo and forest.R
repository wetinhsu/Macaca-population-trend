#---- load library
library(data.table)
library(readxl)
library(writexl)
library(tidyverse)
library(sf)
library(magrittr)
#-----------------------------
#read point data

M.data <- read_excel("./data/clean/for analysis_V1.xlsx",
                     sheet=1) %>% setDT %>% 
  #.[, DATE := as.IDate(paste(Year, Month, Day, sep = "/"))] %>% 
  .[TypeName %like% "混", TypeName.n := "mixed"] %>% 
  .[TypeName %like% "竹林", TypeName.n := "Bamboo"] %>% 
  .[TypeName %like% "闊葉樹林型", TypeName.n := "broad-leaved"] %>% 
  .[TypeName %like% "針葉樹林型", TypeName.n := "coniferous"] %>% 
  .[, TypeName.1 := ifelse(Distance>20, "Not forest", TypeName.n)] 



M.data$Year %<>% as.numeric
M.data$Survey %<>% as.numeric
M.data$Point %<>% as.numeric
M.data$Macaca_sur %<>% as.numeric
M.data$Month %<>% as.numeric
M.data$Day %<>% as.numeric
M.data$Distance %<>% as.numeric
M.data$julian.D %<>% as.numeric
M.data$Region %<>% as.factor
M.data$TypeName.1 %<>% as.factor
M.data$Site_N %<>% as.factor
M.data$Region2 %<>% as.factor

#---------------------------------------------------------------------

M.data <- M.data %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>% 
  .[, Year := as.numeric(Year)] %>% 
  .[, Year.re := Year - min(Year) + 1]



#==============================================
df <- 
  M.data %>% 
  #.[Year < 2019,] %>%
  .[!(TypeName.1 %in% "Not forest"), ] %>% 
  .[Macaca_dist %in% "c", Macaca_sur :=0]

#----------------
#read forest spatial data, merge polgons by TypeName, area by TypeName

path <- "D:/R/test/第四次森林資源調查全島森林林型分布圖"

nc <- st_read(paste0(path,"/","全島森林林型分布圖.shp"),
              crs="+init=epsg:3826") %>% 
  arrange(TypeName, st_geometry_type(geometry), FunctionTy,Function)

nc.a<-
  nc$TypeName %>% 
  as.character %>%
  unique %>% 
  lapply(.,
         function(x){
           tmp <- 
             nc %>%
             filter(TypeName %in% x) %>% 
             st_combine() 
           
           st_sf(TypeName= x, geom = st_sfc(tmp), crs = 3826)
         }) %>% 
  do.call(rbind,.)


bb <- 
  nc %>%
  st_drop_geometry %>% 
  group_by(TypeName) %>%
  summarise(area = sum(Area_ha) %>% round(.,4))

nc.b <- 
  left_join(nc.a, bb) %>% 
  filter(!TypeName %in% c("待成林地", "裸露地", "陰影","竹林"))

#-------------------------------------------
M.bomboo <- 
df %>% 
  filter(TypeName.1 %in% "Bamboo") %>% 
  filter(Macaca_sur %in% 1) %>% 
  mutate(pointid = paste0(Site_N,"-",Point)) %>% 
  select(pointid,X,Y,Distance) %>% 
  unique() %>% 
  st_as_sf(., coords = c("X", "Y"), crs = 4326) %>% 
  st_transform(., 3826)

st.dis2<-st_distance(M.bomboo,nc.b)
TypeName <-apply(st.dis2,1,which.min) %>% nc.b$TypeName[.] %>% as.character
distance<-apply(st.dis2,1,min)

M.bomboo$TypeName2 <- TypeName
M.bomboo$Dist2 <- distance


ggplot(nc.b)+geom_sf(aes(fill = TypeName, color =TypeName))+
  geom_sf(data = M.bomboo)

