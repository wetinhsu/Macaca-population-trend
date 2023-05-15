library(tidyverse)
library(sf)
library(openxlsx)

#-----------

tt <- 
st_read("D:/待處理工作夾(做完要歸檔)/國土利用104/MOILandUse104.gdb")

tt.1 <- 
tt %>% 
  st_drop_geometry() %>% 
  select(LCODE_C2) %>% 
  unique() 

Land_types <- 
  tt.1 %>%    #分類參考大利博論的附錄2-2
  mutate(types = LCODE_C2) %>% 
  mutate(types = str_replace_all(types, c("0101|0102" = "Farmland",
                                          "0201|0202|0204" = "Forest",
                                          "03\\d{2}|05\\d{2}|06\\d{2}|0402|0409|0701|0702|0703|0802" = "Built area",
                                          "0104|0401|0403|0404|0405|0406" = "Water body")))%>% 
  mutate(types = str_replace_all(types, "\\d{4}", "Others"))

pp <- 
read.xlsx("./研討會_202307/data/refer/Dali/Ch2_BBS100mbuffer_coverage1217.xlsx",
          sheet = "bbs") %>% 
  select(1:3) %>% 
  filter(!x %in% "-") %>% 
  filter( !is.na(x)) %>% 
  unique()

#-------------------
library(raster)



str_name<-'D:/R/test/不分幅_全台及澎湖/dem_20m.tif' 
imported_raster=raster(str_name )

s <- stack(imported_raster)

ele<-
  pp %>%
  as.data.frame(.) %>% 
  st_as_sf(., coords = c("x", "y"), crs = 4326) %>% 
  st_transform(crs(imported_raster)) %>%
  extract(imported_raster, ., method='simple') %>% 
  data.frame(site=pp$site, ELE = .)%>% 
  mutate(trancect = str_sub(site, 1,6)) %>% 
  group_by(trancect) %>% 
  summarise(elevation = median(ELE))  #樣點海拔的中位數


#----------------------------

point_buffer <-   # buffer 取100m
st_as_sf(pp, coords = c("x", "y"), crs = 4326) %>% 
  st_transform(3826) %>% 
  mutate(trancect = str_sub(site, 1,6)) %>% 
  st_buffer(., dist = 100) %>%  
  group_by(trancect) %>% 
  summarise()


tt3 <-   #用樣點的buffer切下國土圖層
st_intersection(point_buffer, tt)


summary(tt3)



tt4 <-  #計算樣點內各土地類型的面積及周長
tt3 %>% 
  st_buffer(dist=0) %>% 
  group_by(trancect,LCODE_C2) %>%
  summarise() %>% 
  split(., row.names(.)) %>% 
  map(function(x){
    
    x %>% 
      mutate(AA = st_area(.) %>% sum) %>% 
      mutate(LL = st_length(st_cast(.,"MULTILINESTRING"))) 
    
  })%>% 
  bind_rows(.) 
  

tt4 %>%  #挑一個樣區來畫圖看看
  left_join(., Land_types) %>% 
  filter(trancect %in% "A37-11") %>% 
  ggplot(.)+
  geom_sf(aes(fill = types), show.legend = T)


tt5 <-  #由樣點的各土地類型的面積及周長，加總成樣區的面積及周長
tt4 %>% 
  st_drop_geometry() %>% 
  mutate(AA = AA %>% as.numeric(),
         LL = LL %>% as.numeric()) %>% 
  arrange(trancect, LCODE_C2) %>% 
  left_join(., Land_types) %>% 
  group_by(trancect, types) %>% 
  summarise(sum_AA = sum(AA),
            sum_LL = sum(LL)) 

tt6 <-  #由樣區的各類型的面積、總面積及周長總和
tt5 %>% 
  bind_rows( 
    tt5 %>%
      group_by(trancect) %>% 
      summarise(sum_AA = sum(sum_AA),
                sum_LL = sum(sum_LL)) %>% 
      mutate(types = "total")) %>% 
  
  reshape2::melt(id = 1:2) %>% 
  reshape2::dcast(trancect ~ types + variable, value.var = "value") %>% 
  dplyr::select(str_subset(colnames(.), "trancect|sum_AA|total")) %>% 
  setNames(., str_remove_all(colnames(.), "_sum_AA")) %>% 
  setNames(., str_replace_all(colnames(.), "total_sum_LL", "length"))
  
  
tt7 <- #添加樣區的各類型面積的占比、edge破碎度、海拔
  tt6 %>% 
  mutate(P_forest = Forest/total,
         P_farmland = Farmland/total,
         P_water = `Water body`/total,
         P_built = `Built area`/total,
         P_others = Others/total,
  ) %>% 
  mutate(edge = length*100/total) %>% 
  mutate(Shannon = -(
           ifelse(is.na(P_forest),0,P_forest*log(P_forest))+ 
             ifelse(is.na(P_farmland),0,P_farmland*log(P_farmland))+ 
             ifelse(is.na(P_water),0,P_water*log(P_water))+ 
             ifelse(is.na(P_built),0,P_built*log(P_built))+ 
             ifelse(is.na(P_others),0,P_others*log(P_others))  )
           ) %>% 
  left_join(ele, by = "trancect")


  