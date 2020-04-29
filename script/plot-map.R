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
                                    ifelse(TypeName %in% "竹林", "竹林", "混淆林"))))


#---------------------

S.all_M<- S.all[Macaca_sur %in% 1,] %>%
  .[,list(Year, X, Y)]%>%
  unique() %>% 
  setDF  #monkey data

S.all_P<- S.all%>%
  .[!(TypeName.1%in% "非森林"),] %>% 
  .[,list(X, Y)]%>%
  
  unique() %>% 
  setDF  #Point data


#plot --------------------------------------------------------------------


ggplot()+

  geom_sf(data = nc.b, aes(fill = TypeName.1), color = NA, size = 1)+ 
  
  geom_sf(data = TW, fill = NA, color = gray(.5))+
  
  geom_point(data = S.all_P,
             aes(x = X, y = Y,  shape = "B"),
             fill = "#9EC6FF",
             size = 2,
             alpha = 1)+
  
  geom_point(data = S.all_M,
             aes(x = X, y = Y,  shape = "A"),
             fill ="red",
             size = 2,
             alpha = 0.7)+
  
  scale_shape_manual(values = c("A" = 21, "B" = 21),
                     labels = c("Monkey troop", "Samplig point"),
                     name = "",
                     guide = guide_legend(order = 1,
                                          override.aes = list( fill = c("red", "#9EC6FF"),
                                                               size = c(3, 3)),
                                          title.theme = element_blank(),
                                          label.theme = element_text(family="serif",
                                                                     face = "bold",
                                                                     size = 13)))+
  
  scale_fill_manual(values = c("闊葉林" = "#99CC99", 
                               "針葉林" = "#009966", #深綠
                               "竹林" = "#FFFF99", #黃
                               "混淆林" = "#FF9966"),#橘
                    
                    labels = c("闊葉林" = "Broadleaf",
                               "針葉林" = "Coniferous",
                               "竹林" = "Bamboo",
                               "混淆林" = "Mixed"),
                    name = "Forest Type",
                    guide = guide_legend(order = 2))+
  
  
  coord_sf(crs = 4326, 
           xlim = c(119.5, 122.5), ylim = c(21.5, 25.45),
           expand = FALSE)+
  
  scale_x_continuous(breaks = 120:122)+
  scale_y_continuous(breaks = 22:25)+
  
  annotation_north_arrow(location = "tl",
                         which_north = "grid",
                         height = unit(1.0, "cm"),
                         width = unit(1.0, "cm"),
                         pad_x = unit(1.5, "cm"),
                         pad_y = unit(1.5, "cm"),
                         style = north_arrow_orienteering())+
  annotation_scale(location = "bl",
                   pad_x = unit(1.5, "cm"),
                   pad_y = unit(1.2, "cm"),
                   style = "ticks") +
  
  theme_bw()+
  theme(
    aspect.ratio = 1.35,
    text = element_text(family="serif"),

    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.title = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    plot.margin = margin(0,0,0,0),
    
    
    
    legend.justification = c(1,0),
    legend.position = c(0.95,0.05),
    legend.text = element_text(size = 12),
    legend.title = element_text(face = "bold", size = 15),
    legend.box.margin = margin(0,0,0,0)
  ) 



ggsave("MAP_5.png",
           path = "./result",
           width = 15,
           height = 19,
           units = "cm")

ggplot()+
  geom_point(data = S.all,aes(x=Altitude, y = Y))+
  geom_point(data = S.all[Macaca_sur %in% 1,],aes(x=Altitude, y = Y),col = 'red')

ggplot()+
  geom_point(data = S.all,aes(x=X, y = Altitude))+
  geom_point(data = S.all[Macaca_sur %in% 1,],aes(x=X, y = Altitude),col = 'red')



