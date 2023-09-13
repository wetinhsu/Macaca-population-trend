library(tidyverse)
library(sf)
library(openxlsx)

#-----------

tt <- 
st_read("D:/待處理工作夾(做完要歸檔)/國土利用104/MOILandUse104.gdb")%>% 
  st_buffer(dist=0) 

Used_Land_types <- 
  c("0101","0102", #Farmland
                         "0103", #orchard
                         "0201","0202","0204", #Forest
                         "0301","0302","0303","0304", #Built area
                         "0501","0502","0503","0504", #Built area
                         "0601","0602","0603","0604","0605","0606", #Built area
                         "0402","0409","0701","0702","0703","0802", #Built area
                         "0104","0401","0403","0404","0405","0406", #Water body
                         "0801", "0901", "0902", "0903", "0904", "0905")


Land_types <-  
  data.frame(LCODE_C2 = tt$LCODE_C2)%>% 
  unique()  %>%   
  
  filter(LCODE_C2 %in% Used_Land_types) %>% 
  
  mutate(types = LCODE_C2) %>%  #分類參考大利博論的附錄2-2
  mutate(types = str_replace_all(types, c("0101|0102|0103" = "Farmland",
                                          "0201|0202|0204" = "Forest",
                                          "03\\d{2}|05\\d{2}|06\\d{2}|0402|0409|0701|0702|0703|0802" = "Built area",
                                          "0104|0401|0403|0404|0405|0406" = "Water body")))%>% 
  mutate(types = str_replace_all(types, "\\d{4}", "Others"))


pp <- 
read.xlsx("./研討會_202307/有林務局資料/data/clean/for analysis_1522_v1.xlsx") %>% 
  dplyr::select(Site_N, Point, X_97, Y_97, Altitude) %>% 
  unique()

#-------------------
library(data.table)

ele<-
  pp %>% setDT %>% 
  .[,transect := Site_N] %>% 
  .[, .(elevation = median(Altitude)), by = transect] #樣點海拔的中位數



#----------------------------

point_buffer <-   # buffer 取500m
st_as_sf(pp, coords = c("X_97", "Y_97"), crs = 3826) %>% 
  mutate(transect = Site_N) %>% 
  st_buffer(., dist = 500) %>%  
  group_by(transect) %>% 
  summarise()

tt3 <-   #用樣點的buffer切下國土圖層
  tt %>%   
  filter(LCODE_C2 %in% Used_Land_types)%>% 
  st_intersection(point_buffer, .)


summary(tt3)



tt4 <-  #樣區的各土地類型的面積及周長
tt3 %>% 
  left_join(., Land_types) %>% 
  group_by(transect,types) %>%
  summarise() %>% 
  split(., row.names(.)) %>% 
  map(function(x){
    
    x %>% 
      mutate(AA = st_area(.) %>% sum) %>% 
      mutate(LL = st_length(st_cast(.,"MULTILINESTRING"))) 
    
  })%>% 
  bind_rows(.)  %>% 
  mutate(AA = AA %>% as.numeric(),
         LL = LL %>% as.numeric()) 
  

tt4 %>%  #挑一個樣區來畫圖看看
  filter(transect %in% "A01-02") %>% 
  ggplot(.)+
  geom_sf(aes(fill = types), show.legend = T)


tt5 <-  #由樣點的各土地類型的面積及周長，加總成樣區的面積及周長
tt4 %>% 
  st_drop_geometry() %>% 
  setDT

tt6 <-  #由樣區的各類型的面積、總面積及周長總和
tt5 %>% 
  bind_rows( 
    tt5 %>%
      .[,.(AA = sum(AA, na.rm =T),
           LL = sum(LL, na.rm =T)), by = transect] %>% 
      .[, types := "total"] )%>% setDT %>%
  reshape2::melt(id = 1:2) %>% 
  reshape2::dcast(transect ~ types + variable, value.var = "value") %>% 
  .[,str_subset(colnames(.), "transect|AA|total")]%>% 
  setNames(., str_remove_all(colnames(.), "_AA")) %>% 
  setNames(., str_replace_all(colnames(.), "total_LL", "edge_length"))
  
  
tt7 <- #添加樣區的各類型面積的占比、edge破碎度、海拔
  tt6 %>% setDT %>% 
  .[, P_forest := Forest/total] %>% 
  .[, P_farmland := Farmland/total] %>% 
  .[, P_water := `Water body`/total] %>% 
  .[, P_built := `Built area`/total] %>% 
  .[, P_others := Others/total] %>% 
  .[, edge := `edge_length`*100/total] %>% 

  .[, Shannon := -(
           ifelse(is.na(P_forest),0,P_forest*log(P_forest))+ 
             ifelse(is.na(P_farmland),0,P_farmland*log(P_farmland))+ 
             ifelse(is.na(P_water),0,P_water*log(P_water))+ 
             ifelse(is.na(P_built),0,P_built*log(P_built))+ 
             ifelse(is.na(P_others),0,P_others*log(P_others))  )
           ] %>% 
  left_join(ele, by = "transect")


write.xlsx(tt7,"./研討會_202307/有林務局資料/data/clean/BBSsite500mbuffer_0728.xlsx")
