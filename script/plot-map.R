#---- load library

library(data.table)
library(readxl)
library(writexl)
library(tidyverse)
library(sf)
library(ggspatial)
library(ggrepel)
#-------------

EL50 <- st_read("./data/clean/gis/elev50.shp") %>% 
  st_transform(3826)

summary(EL50)
ggplot(EL50)+geom_sf()

#-----------------------------------------------------
#county
path2 <- "D:/R/test/COUNTY_MOI_1081121"
path2 <- "//10.40.1.138/Bird Research/BBSTW/瑋婷_資料暫存(硬碟備份)/圖層(基礎資料)/COUNTY_MOI_1081121"
TW <- st_read(paste0(path2,"/","COUNTY_MOI_1081121.shp")) %>% 
  st_transform(.,3826) %>% 
  filter(!COUNTYNAME %in% c("連江縣", "澎湖縣","金門縣")  )%>% 
  mutate(Region = ifelse(COUNTYNAME %in% c("宜蘭縣", "臺北市", "基隆市", "新北市", "新竹市", "新竹縣", "桃園市", "苗栗縣"), "Northern",
                         ifelse(COUNTYNAME %in% c("臺中市", "彰化縣", "南投縣"), "Central",
                                ifelse(COUNTYNAME %in% c("雲林縣", "嘉義市", "嘉義縣", "臺南市"), "Southcenterern",
                                       ifelse(COUNTYNAME %in% c("高雄市", "屏東縣"), "Southern",
                                              ifelse(COUNTYNAME %in% c("花蓮縣"), "Hualien", "Taitung")))))) 
st_transform(TW,4326)

Region <- 
TW$Region %>% 
  as.character %>%
  unique %>% 
  lapply(.,
         function(x){
           tmp <- 
             TW %>%
             filter(Region %in% x) %>% 
             st_union() 
           
           st_sf(Region= x, geometry = st_sfc(tmp), crs = 3826)
         }) %>% 
  do.call(rbind,.)

ggplot(Region)+geom_sf()
#----------------
#read forest spatial data, merge polgons by TypeName, area by TypeName

path <- "D:/R/test/第四次森林資源調查全島森林林型分布圖"
path <- "//10.40.1.138/Bird Research/BBSTW/瑋婷_資料暫存(硬碟備份)/圖層(基礎資料)/第四次森林資源調查全島森林林型分布圖"
nc <- st_read(paste0(path,"/","全島森林林型分布圖.shp")) %>% 
  st_transform(3826) %>% 
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
#read point data

S.all<- 
  read_xlsx("./data/clean/for analysis_V1.xlsx") %>%setDT  %>% 
  .[, Year := as.character(Year)]



S.all_M<- S.all %>% 
  filter(Macaca_sur %in% 1) %>%
  filter(analysis %in% "Y") %>% 
  filter(!TypeName.1 %in% "非森林") %>% 
  select("X", "Y")%>%
  unique()  #monkey data

S.all_P<- S.all %>% 
  filter(analysis %in% "Y") %>% 
  filter(!TypeName.1 %in% "非森林") %>% 
  select("X", "Y")%>%
  unique()  #Point data



#plot --------------------------------------------------------------------


ggplot()+

  geom_sf(data = nc.b, aes(fill = TypeName.1), color = NA, size = 1)+ 
  
#  geom_sf(data = TW, fill = NA, color = "#ADADAD", size = .9)+
  
  geom_sf(data = Region, fill = NA, aes(color = "#7B7B7B"), size = .5)+
  
  geom_rect(aes(xmin = 121.45, xmax = 121.66, ymin = 21.92, ymax = 22.69), fill = "white")+
  
  geom_label_repel(data = Region, aes(label = Region, geometry = geometry),
                   fill = NA,
                   size = 5, 
                   fontface = "bold",
                   stat = "sf_coordinates",
                   min.segment.length = 0, 
                   segment.colour = NA,
                   label.size = NA,
                   box.padding = 0,
                   nudge_x      = c(-0.8,-0.7,-0.9,-0.4,0.4,0.5),
                   nudge_y      = c(0.1,0.2,-.05,-0.3,-0.2,0)) +
  
#  geom_sf(data = EL50, fill = NA, color = "#FF9EFF", size = .1, lty = 1)+ 
  
  
  geom_point(data = S.all_P,
             aes(x = X, y = Y,  shape = "B"),
             color = "#72A8F8",
             size = 2)+
  
  geom_point(data = S.all_M,
             aes(x = X, y = Y,  shape = "A"),
             fill ="red",
             size = 2,
             alpha = 0.7)+
  
  scale_shape_manual(values = c("A" = 21, "B" = 16),
                     labels = c("Sampling points with macaque troop",
                                "Sampling point without macaque troop"),
                     name = NA,
                     guide = guide_legend(order = 1,
                                          override.aes = list( fill = c("red", NA),
                                                               color = c("black", "#72A8F8"),#""
                                                               size = c(3, 3)),
                                          title.theme = element_blank(),
                                          label.theme = element_text(family="serif",
                                                                     face = "bold",
                                                                     size = 12)))+
  scale_color_manual(values = c("#7B7B7B"),
                     labels = c("Region"),
                     name = NA,
                     guide = guide_legend(order = 2,
                                          title.theme = element_blank()))+
  
  scale_fill_manual(values = c("闊葉林" = "#99CC99", 
                               "針葉林" = "#009966", #深綠
                               "竹林" = "#FF9999",#"#FFFF99", #黃
                               "混淆林" = "#FF9966"),#橘
                    
                    breaks = c("闊葉林", 
                               "針葉林",
                               "竹林",
                               "混淆林"),
                    
                    labels = c("Broadleaved forest",
                               "Conifer forest",
                               "Bamboo forest",
                               "Mixed forest"),
                    name = NA,
                    guide = guide_legend(order = 3,
                                         title.theme = element_blank()))+
  
  
  coord_sf(crs = 4326, 
           xlim = c(119, 123), ylim = c(21, 25.45),
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
  #  aspect.ratio = 1.35,
    text = element_text(family="serif"),

    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.title = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    plot.margin = margin(0,0,0,0),
    
    
    legend.background = element_blank(),
    legend.justification = c(0.7,0.1),
    legend.position = c(0.85,0.05),
    legend.text = element_text(size = 12),
    legend.spacing = unit(0, 'mm'),
    legend.margin = margin(0,0,0,0),
    legend.box.background = element_blank()
  ) 



ggsave("MAP_12.png",
           path = "./result",
           width = 15,
           height = 19,
           units = "cm")


