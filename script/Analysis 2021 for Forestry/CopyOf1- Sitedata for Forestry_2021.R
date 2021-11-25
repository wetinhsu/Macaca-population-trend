#樣點資料檢核
#---- load library

library(tidyverse)
library(readxl)
library(writexl)
library(sf)

#------------

M.Point <- 
  read_excel("./data/refer/林務局獼猴樣點_20211118.xlsx") %>% 
  filter(!is.na(TWD97_X)|!is.na(TWD97_Y)) 
  



st_M.Point <-
  M.Point %>% 
  mutate(X = as.numeric(TWD97_X)) %>% 
  mutate(Y = as.numeric(TWD97_Y)) %>%
  st_as_sf(., coords = c("X", "Y"), crs = 3826)




#--------------

DF<- 
lapply(paste0("./data/raw/FORESTRYdata/", 2021), function(x){
  list.files(x, full.names = T) %>%  #讀取各2020資料夾內的excel檔名
    lapply(., function(x){
      x %>% 
      excel_sheets(.) %>%   #讀取各excel內的sheet名
      lapply(.,read_excel, path = x,
             col_names = T, col_type = "text", cell_cols("B:S")) %>%  #讀取各sheet的內容
      bind_rows()  
    })
  }) %>% 
  bind_rows() %>% 
  select(Office = `林管處`, 
         Station = `工作站`,
         Site_N = `樣區編號`, 
         Year = `年`, 
         Month = `月`, 
         Day = `日`, 
         Survey = `旅次`, 
         Surveyor = `調查者`,
         Point = `樣點編號`, 
         TWD97_X = `X座標(TWD97)`, 
         TWD97_Y = `Y座標(TWD97)`, 
         Hour = `時`, 
         Minute = `分`, 
         Macaca_sur = `數量`, 
         Macaca_dist = `距離`, 
         Macaca_voice = `叫聲`, 
         Habitat = `棲地類型(主要)`) %>% 
  mutate(Surveyor = gsub(",","、", Surveyor)) %>%
  mutate(Surveyor = gsub(" ","", Surveyor)) %>%
  mutate(Macaca_voice = gsub("n","N", .$Macaca_voice)) %>% 
  mutate(Macaca_voice = ifelse(Macaca_sur %in% 0 & Macaca_voice %in% c("N"), NA,  Macaca_voice)) %>% 
  
  mutate(Date = ISOdatetime(Year, Month, Day, Hour, Minute, sec = 0) ) %>% 
  mutate(Office = ordered(Office, c("羅東", "新竹", "東勢", "南投", "嘉義", "屏東", "花蓮", "臺東")))


DF %>% 
  filter(Macaca_sur %in% 0) %>% 
  filter(! is.na(Macaca_dist) | !is.na(Macaca_voice) ) 

DF %>% 
  filter( Macaca_sur %in% c(1, 2)) %>% 
  filter( is.na(Macaca_dist) | is.na(Macaca_voice) ) 


#確認旅次---
DF <- 
DF %>%  
  reshape2::dcast(Year + Site_N ~ Survey, length, value.var = "Macaca_sur") %>% 
  #計算第1旅次及第2旅次調查的樣點數
  filter(`1` %in% 0 & `2` >= 6) %>%  #找出有第2旅次沒第1旅次的樣區
  left_join(DF, ., by = c("Year", "Site_N"))%>%
  mutate( Survey = ifelse(!is.na(`1`),1, Survey)) %>%#將第1次調查的旅次改回1
  select(-`1`, -`2`, -`NA`)




#Part 1 最粗的資料----
#(收到的資料)----------------------

#樣區樣點的統計資料

Count_Point_Survey <- 
DF %>% 
  filter(!is.na(Macaca_sur) ) %>% 
  group_by(Office, Survey) %>% 
  summarise(Site_n = Site_N %>% unique %>% length,
            Data_n = n()) %>% 
  reshape2::melt(id = 1:2) %>% 
  reshape2::dcast(Office ~ Survey + variable, guess_var = "value")

#有些樣點兩旅次做在不同位置上，所以data數不會剛好是point的兩倍
#Data_n奇數筆原因是花蓮的長良林道，第1旅次7樣點，第2旅次刪除沒作，另設長良樣區(8樣點)只作1旅次。


#Part 2 刪疏失的資料----------------
#(刪除不足6分鐘、 after11pm、不在檢核點上、同一旅次超過7日才完成調查)----------
#同一旅次同一樣區內超過7日才完成調查，整個旅次的資料方棄。----

 DF.2 <- 
   DF %>% 
   add_column(analysis = "Y", .before = "Office") %>% 
  

   mutate(SS = paste0(Site_N, "-", Survey)) %>% 
  
  split(., .$Site_N) %>% 
  lapply(., function(x){  #檢核時間
    x %>% 
      split(., .$Survey) %>% 
      lapply(., function(y){
        
        y %>% 
          arrange(Date) %>% 
          mutate(time_2=  c(Date[2: n()], NA))%>% 
       mutate(time_diff = difftime(time_2, Date, units='mins') %>% 
                as.numeric())  %>%   
       
       mutate(analysis = ifelse(time_diff<6 & !is.na(time_diff),  paste0(analysis, ", 6min"), analysis)) %>% 
       
       select(-time_2) %>% 
       
       mutate(day_diff = difftime(Date[n()], Date[1], units='days') %>% 
                as.numeric() %>% round(.,0))  %>% 
       mutate(analysis = ifelse(day_diff > 7, 
                          paste0(analysis, ", 7day"), analysis)) %>% 
       
       mutate(analysis = ifelse(as.numeric(Hour) >= 11, 
                           paste0(analysis, ", Toolate"), analysis))
      }) %>% 
      bind_rows()}) %>%   
  bind_rows() %>% 
  
  split(., .$SS) %>% 
  lapply(., function(x){  #計算位置誤差距離：依樣區旅次分別計算各樣區內，各樣點離做近檢核點的距離
     
     st_M.Point <-  #與st_DF對應的樣區
       st_M.Point %>% 
       filter(Macaca_Site %in% unique(x$Site_N)) 
     
     st_DF.2 <-   
       x %>% 
       mutate(SP = ifelse(!nchar(Point) %in% 1,
                          paste0(Site_N, "-0", Point),
                          paste0(Site_N, "-", Point))) %>%
       
       mutate(X = as.numeric(TWD97_X)) %>% 
       mutate(Y = as.numeric(TWD97_Y)) %>% 
       st_as_sf(., coords = c("X", "Y"), crs = 3826)
     
     st_DF.2$point_O <-
       st_distance(st_DF.2, st_M.Point) %>% 
       apply(.,1,which.min) %>% 
       st_M.Point$樣區樣點編號[.] 
     
     st_DF.2$point.diff <- 
       st_distance(st_DF.2, st_M.Point) %>% 
       apply(., 1,min) 
     
     return(st_drop_geometry(st_DF.2))}) %>% 
   
   
    bind_rows() %>% 
  select(-SS) 





DF.2%>%   #刪除輔註欄位
  
  mutate(analysis =case_when(
                             analysis %in% "Y" & point.diff > 50                   ~ "N3",  # range of gps =50m
                             analysis %in% "Y" & Point %in% "X"                    ~ "N3",  # range of gps =50m
                             TRUE ~ analysis)) %>% 
  View
         




 DF.2 %>%  #各林管處的各疏失情況的樣點數
  group_by(Office, analysis) %>%
  summarise(N = n()) %>%
  reshape2::dcast( analysis ~ Office, guess.var = "N")
 
 
 DF.2 %>%   #林管處無疏失情形的樣點數
   filter(!is.na(Macaca_sur) ) %>% 
   filter(analysis %in% "Y" ) %>% 
   group_by(Office, analysis, Survey) %>%
   summarise(N = n()) %>%
   reshape2::dcast( Office ~ analysis + Survey, guess.var = "N")
 

 
 
 DF.2 %>%    #時無疏失情形下，林管處在1、2旅次的猴群、孤猴數
   filter(analysis %in% "Y")  %>% 
   filter(!is.na(Macaca_sur) ) %>% 
   group_by(Office, Macaca_sur, Survey) %>% 
   summarise(N = n()) %>% 
   reshape2::dcast(Office + Survey ~ Macaca_sur, guess_value  = "N")


 DF.2 %>% 
   filter(analysis %in% "Y")  %>% 
   filter(!is.na(Macaca_sur) ) %>% 
   left_join(st_drop_geometry(st_M.Point), by = c("point_O" = "樣區樣點編號") ) %>% 
   mutate(Macaca_sur = ifelse(Macaca_sur %in% "1" , 0,
                              ifelse(Macaca_sur %in% "2" , 1, Macaca_sur))) %>% 
   mutate(Macaca_sur = as.numeric(Macaca_sur)) %>% 
   group_by(Office, Macaca_sur, TypeName.1) %>% 
   summarise(N = n()) %>%
   reshape2::dcast( Office + Macaca_sur ~ TypeName.1, guess.var = "N")
 
 M.data <- 
   DF.2 %>% 
   filter(analysis %in% "Y")  %>% 
   filter(!is.na(Macaca_sur) ) %>% 
   left_join(st_drop_geometry(st_M.Point), by = c("point_O" = "樣區樣點編號") ) %>% 
   select(analysis, Office, Station, 
          "Site_N" = Macaca_Site,
          "Point" = 樣點代號, 
          "X" = TWD97_X.y,
          "Y" = TWD97_Y.y,
          Year, Month, Day, Survey, Macaca_sur,
          Hour, Minute, Day, 
          Macaca_dist,
          Macaca_voice,
          Habitat,
          "Distance" = distance, 
          "TypeName" = join_TypeName, TypeName.1, Altitude)

 M.data <-
 M.Point %>% 
   filter(str_detect(備註, "刪除")) %>% 
   select(Macaca_Site, 樣點代號) %>% 
   
   anti_join(M.data, ., by = c( "Site_N" = "Macaca_Site", "Point" = "樣點代號")) 
 
 
 
 #猴群、孤猴的統計資料             
 
 M.data  %>% 
   mutate(SP = paste0(Site_N, "-", Point))  %>% 
   filter(!is.na(Macaca_sur) ) %>% 
   group_by(Office, Macaca_sur) %>% 
   summarise(N = SP %>% length) %>% 
   reshape2::dcast(Office ~ Macaca_sur, guess_value  = "N")
 
 
 NOTE <- data.frame(說明 = "本資料集為林務局獼猴調查資料的合併檔，並已完成清理動作，僅留下符合調查規範的資料。共計4178筆。")
 
 
  write_xlsx( list('NOTE' = NOTE, 'Data' = M.data), "./data/clean/full_combind_Forestrydata_2021_V1.xlsx")
 
 #Part 3 可納入分析的資料----------------
#(僅留下 距離A、B、海拔50m以上、森林、300m的猴群)---------- 


 
M.Point %>% 
    filter(str_detect(備註, "刪除")) %>% 
    select(Macaca_Site, 樣點代號) %>% 

  anti_join(M.data, ., by = c( "Site_N" = "Macaca_Site", "Point" = "樣點代號")) %>% 
    View
  
  