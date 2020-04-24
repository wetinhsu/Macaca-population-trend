#---- load library

library(data.table)
library(readxl)
library(writexl)
library(tidyverse)
library(sf)
library(ggspatial)
#-------------

#read point data

S.all<- 
    read_xlsx("./data/clean/for analysis_V1.xlsx") %>%setDT  %>% 
  .[, Year := as.character(Year)]

#transform to spatial data
st.all<- 
  S.all %>% 
  .[! County %in% c("連江縣", "澎湖縣","金門縣"),] %>% 
  .[, list(x=X, y=Y, X, Y)] %>% 
  unique %>% 
  .[, NO := 1 : nrow(.)] %>% 
  .[, X := as.numeric(X)] %>% 
  .[, Y := as.numeric(Y)] 



#-----------------------------------------------------
#county
path2 <- "D:/R/test/COUNTY_MOI_1081121"

TW <- st_read(paste0(path2,"/","COUNTY_MOI_1081121.shp")) %>% 
  filter(!COUNTYNAME %in% c("連江縣", "澎湖縣","金門縣")  )
st_transform(TW,4326)


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
  filter(!TypeName %in% c("待成林地", "裸露地", "陰影")) %>% 
  mutate(TypeName.1 = ifelse(TypeName %in% "闊葉樹林型", "闊葉林",
                             ifelse(TypeName %in% "針葉樹林型", "針葉林",
                                    ifelse(TypeName %in% "竹林", "竹林", "混淆林")))) %>% 
  arrange(match(TypeName.1, c("闊葉林","針葉林","竹林", "混淆林")))


#---------------------

S.all_M<- S.all[Macaca_sur %in% 1,] %>%
  .[,list(Year, X, Y)]%>%
  unique() %>% 
  setDF  #monkey data

ggplot()+
  geom_sf(data = TW,  alpha = 0)+
  geom_sf(data = nc.b, aes(fill = TypeName.1, color = TypeName.1))+ 

  geom_sf(data = TW, alpha = 0)+
  geom_point(data = S.all_M,
             aes(x = X, y = Y), shape = 21, size = 2, color = "black", fill ="red" , alpha = 0.7)+
  
  
  coord_sf(xlim = c(119.5, 122.5), ylim = c(21.5, 25.5), expand = FALSE)+
  
  scale_x_continuous(breaks = c(120:122))+
  scale_y_continuous(breaks = c(22:25))+
  
  annotation_north_arrow(location = "tl",
                         which_north = "grid",
                         height = unit(1.0, "cm"), width = unit(1.0, "cm"),
                         style = north_arrow_orienteering())+

  
  scale_fill_manual(values = c(`闊葉林` = "#98FB98",
                               `針葉林` = "#1D8641",
                               `竹林` = "#E6D933",
                               `混淆林` = "#FF7F00"),
                    name = "Type of Forest")+
  scale_colour_manual(values = c(`闊葉林` = "#98FB98",
                               `針葉林` = "#1D8641",
                               `竹林` = "#E6D933",
                               `混淆林` = "#FF7F00"),
                    name = "Type of Forest")+
  
  
  theme_bw()+
  theme(
    plot.margin = margin(30,30,20,20),
    text = element_text(family="serif"),
    panel.border = element_rect(size = 1.5),
    
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.title = element_blank(),
    axis.line.x = element_line(color = "black", size = .2),
    axis.line.y = element_line(color = "black", size = .2),
    axis.ticks.length = unit(0.25,"cm"),
    axis.text = element_text(size = 15, hjust = .5, vjust = .5),

    legend.justification = c(1,0),
    legend.position = c(0.9,0.05),
    legend.text = element_text(size = 15)
    ) 


  
ggplot()+
  geom_point(data = S.all,aes(x=Altitude, y = Y))+
  geom_point(data = S.all[Macaca_sur %in% 1,],aes(x=Altitude, y = Y),col = 'red')

ggplot()+
  geom_point(data = S.all,aes(x=X, y = Altitude))+
  geom_point(data = S.all[Macaca_sur %in% 1,],aes(x=X, y = Altitude),col = 'red')


