library(tidyverse)
library(sf)
library(openxlsx)

#-----------
siteBuffer100 <- 
read.xlsx("研討會_202307/data/gis_siteBuffer100_0526/siteBuffer100.xlsx") %>% 
mutate(LCODE_C2 = paste0("0", as.character(LCODE_C2)))%>%   
  
  filter(LCODE_C2 %in% c("0101","0102", #Farmland
                         "0201","0202","0204", #Forest
                         "0301","0302","0303","0304", #Built area
                         "0501","0502","0503","0504", #Built area
                         "0601","0602","0603","0604","0605","0606", #Built area
                         "0402","0409","0701","0702","0703","0802", #Built area
                         "0104","0401","0403","0404","0405","0406", #Water body
                         "0801", "0901", "0902", "0903", "0904", "0905")) %>% 
  
  mutate(types = LCODE_C2) %>%  #分類參考大利博論的附錄2-2
  mutate(types = str_replace_all(types, c("0101|0102" = "Farmland",
                                          "0201|0202|0204" = "Forest",
                                          "03\\d{2}|05\\d{2}|06\\d{2}|0402|0409|0701|0702|0703|0802" = "Built area",
                                          "0104|0401|0403|0404|0405|0406" = "Water body")))%>% 
  mutate(types = str_replace_all(types, "\\d{4}", "Others"))


#-------------------


ele<-
  read.xlsx("./研討會_202307/data/clean/for analysis_1522_v1.xlsx") %>% 
  dplyr::select(Site_N, Point, X_97, Y_97, Altitude) %>% 
  unique() %>%
  mutate(transect = Site_N) %>% 
  group_by(transect) %>% 
  summarise(elevation = median(Altitude))     #樣點海拔的中位數
  


#-----------

V2 <- 
siteBuffer100 %>% 
  group_by(樣區編號, types) %>% 
  summarise(area = sum(area, na.rm = T),
            perimeter = sum(perimeter, na.rm = T)) %>% 
  bind_rows(
    siteBuffer100 %>% 
      group_by(樣區編號) %>% 
      summarise(area = sum(area, na.rm = T),
                perimeter = sum(perimeter, na.rm = T)) %>% 
      mutate(types = "total")
    
  ) %>% 
  reshape2::melt(id = c(1,2)) %>% 
  reshape2::dcast(樣區編號 ~ types + variable, value.var = "value") %>% 
  dplyr::select(str_subset(colnames(.), "樣區編號|_area|total")) %>% 
  setNames(., str_remove_all(colnames(.), "_area")) %>% 
  setNames(., str_replace_all(colnames(.), "total_perimeter", "edge_length"))%>% 
  
  
  mutate(P_forest = Forest/total,
         P_farmland = Farmland/total,
         P_water = `Water body`/total,
         P_built = `Built area`/total,
         P_others = Others/total,
  ) %>% 
  mutate(edge = `edge_length`*100/total) %>% 
  mutate(Shannon = -(
    ifelse(is.na(P_forest),0,P_forest*log(P_forest))+ 
      ifelse(is.na(P_farmland),0,P_farmland*log(P_farmland))+ 
      ifelse(is.na(P_water),0,P_water*log(P_water))+ 
      ifelse(is.na(P_built),0,P_built*log(P_built))+ 
      ifelse(is.na(P_others),0,P_others*log(P_others))  )
  ) %>% 
  left_join(.,ele, by = c("樣區編號" = "transect") )
  


write.xlsx(V2,"./研討會_202307/data/clean/BBSsite100mbuffer_gis_0526.xlsx")
