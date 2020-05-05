#---- load library

library(data.table)
library(readxl)
library(writexl)
library(tidyverse)
library(sf)


#read point data-----------------------------

S.all<- 
  read_xlsx("./data/clean/for analysis_V1.xlsx") %>% setDT  %>% 
  .[, Year := as.character(Year)]

#read forest data----------------
path <- "D:/R/test/第四次森林資源調查全島森林林型分布圖"

nc <- st_read(paste0(path,"/","全島森林林型分布圖.shp"),
              crs="+init=epsg:3826") %>% 
  filter(!TypeName %in% c("待成林地", "裸露地", "陰影")) 



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
  filter(!TypeName %in% c("待成林地", "裸露地", "陰影")) %>% 
  mutate(TypeName.1 = ifelse(TypeName %in% "闊葉樹林型", "闊葉林",
                             ifelse(TypeName %in% "針葉樹林型", "針葉林",
                                    ifelse(TypeName %in% "竹林", "竹林", "混淆林"))))
# distance closest Broadleaf---------------------

S.all_M<- S.all[Macaca_sur %in% 1,] %>% #monkey data
  filter(TypeName.1 %in% "竹林") %>% 
  select(Year, Site_N, Point, Survey, X, Y, TypeName, Distance) %>% 
  st_as_sf(., coords = c("X", "Y"), crs = 4326) %>% 
  st_transform(., 3826)

nc.e <- nc %>% 
  filter(TypeName %in% "闊葉樹林型")

Sys.time()
st.dis2<- st_distance(S.all_M,nc.e)  
Sys.time()

distance<- apply(st.dis2,1,min) #與闊葉林的距離

#------------------
nc.c <- nc %>% 
  filter(!TypeName %in% c("竹林", "竹針混淆林","針葉樹林型"))

summary(nc.c$TypeName)

Sys.time()
st.dis4<- st_distance(S.all_M,nc.c)  
Sys.time()


TypeName4 <- apply(st.dis4,1,which.min) %>% nc.c$TypeName[.] %>% as.character
distance4<- apply(st.dis4,1,min)



#----------------

Sys.time()
st.dis3<- st_distance(S.all_M,nc)  
Sys.time()

TypeName <- apply(st.dis3,1,which.min)


Area <- nc[TypeName,] %>%  #竹林面積
  st_drop_geometry %>% 
  select(Area_ha) 



#------------------
nc.d <- nc %>% 
  filter(!TypeName %in% c("竹林"))

summary(nc.d$TypeName)

Sys.time()
st.dis5<- st_distance(S.all_M,nc.d)  
Sys.time()


TypeName5 <- apply(st.dis5,1,which.min) %>% nc.d$TypeName[.] %>% as.character
distance5<- apply(st.dis5,1,min)


summary(as.factor(TypeName5))


#-------------
nc.d <- nc.b %>% 
  filter(!TypeName %in% c("竹林"))

summary(nc.d$TypeName)

Sys.time()
st.dis6<- 
  st_distance(S.all_M ,nc.d)  
Sys.time()


TypeName6 <- apply(st.dis6,1,which.min) %>% nc.d$TypeName[.] %>% as.character
distance6<- apply(st.dis6,1,min)


df2 <- 
S.all_M%>% 
  st_drop_geometry %>% 
  add_column(.,
             Dist_Broadleaf = distance,
             Type_near2nd = TypeName6,
             Dist_other = distance6)

#---------------------------
df <- 
  S.all_M%>% 
  st_drop_geometry %>% 
  add_column(., Area = Area$Area_ha,
             Dist_Broadleaf = distance,
             Type_near2nd = TypeName4,
             Dist_other = distance4)
write_xlsx(df, "./result/result_Bamboo.xlsx")


ggplot(df, aes(x = Type_near2nd))+
  geom_bar()


ggplot(df, aes(x = Dist_other, group = Type_near2nd))+
  geom_histogram(aes(fill = Type_near2nd),
                 color = gray(.5), bins = 20, position="dodge")+
  stat_bin(binwidth=20, geom="text", aes(label=..count..),
           position="dodge" ) 
