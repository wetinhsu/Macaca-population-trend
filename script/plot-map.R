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


#plot --------------------------------------------------------------------


ggplot()+

  geom_sf(data = nc.b, aes(fill = TypeName.1), color = NA)+ 
  
  geom_sf(data = TW, fill = NA, color = gray(.5))+
  
  geom_point(data = S.all_M,
             aes(x = X, y = Y,  shape = "A"),
             fill ="red",
             size = 2,
             color = "black",
             alpha = 0.7)+
  
  scale_shape_manual(values = c("A" = 21),
                     labels = "Monkey troop",
                     name = "",
                     guide = guide_legend(order = 1,
                                          title.theme = element_blank(),
                                          label.theme = element_text(family="serif",
                                                                     face = "bold",
                                                                     size = 13)))+
  
  scale_fill_manual(values = c("#E6D933",
                               "#1D8641",
                               "#FF7F00",
                               "#98FB98"),
                    breaks = c('闊葉林',
                               '針葉林',
                               '竹林',
                               '混淆林'),
                    labels = c("Broadleaf",
                               "Coniferous",
                               "Bamboo",
                               "Mixed"),
                    name = "Forest Type",
                    guide = guide_legend(order = 2))+
  
  
  coord_sf(crs = 4326, 
           xlim = c(119.5, 122.5), ylim = c(21.5, 25.5),
           expand = FALSE)+
  
  scale_x_continuous(breaks = 120:122)+
  scale_y_continuous(breaks = 22:25)+
  
  annotation_north_arrow(location = "bl",
                         which_north = "grid",
                         height = unit(1.0, "cm"),
                         width = unit(1.0, "cm"),
                         style = north_arrow_orienteering())+
  
  theme_bw()+
  theme(
    aspect.ratio = 1.55,
    plot.margin = margin(5,5,5,5),
    text = element_text(family="serif"),
    panel.border = element_rect(size = 1),
    
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.title = element_blank(),
    axis.ticks.length = unit(0.25,"cm"),
    axis.text = element_text(size = 12, color = gray(.2), 
                             hjust = .5, vjust = .5),
    
    legend.justification = c(1,0),
    legend.position = c(0.95,0.05),
    legend.text = element_text(size = 12),
    legend.title = element_text(face = "bold", size = 15),
    legend.box.margin = margin(0,0,0,0)
  ) 



ggsave("MAP_3.png",
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



