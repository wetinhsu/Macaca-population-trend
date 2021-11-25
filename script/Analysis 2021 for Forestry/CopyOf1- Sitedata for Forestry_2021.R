#樣點資料檢核
#---- load library

library(tidyverse)
library(readxl)
library(writexl)
library(sf)

#------------

M.Point <- 
  read_excel("./data/refer/林務局獼猴樣點_20211118.xlsx") %>% 
  filter(!is.na(TWD97_X)|!is.na(TWD97_Y)) %>% 
  mutate(樣點代號 = as.character(樣點代號)) %>% 
  select(-`樣區樣點編號`)
  



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
          
          select(-time_2) %>% 
          
          mutate(day_diff = difftime(Date[n()], Date[1], units='days') %>% 
                as.numeric() %>% round(.,0))  
        }) %>% 
      bind_rows()
    }) %>%   
  bind_rows() %>%

  left_join(M.Point,
            by = c("Site_N" = "Macaca_Site",
                   "Point" = "樣點代號"),
            suffix = c(".ori", "")) %>% 
  mutate(point_diff = NA) %>% 
  split(. ,rownames(.)) %>% 
  lapply(., function(x){
    
    P.sur <- #調查位置
    x %>%
      mutate(X = as.numeric(TWD97_X.ori)) %>% 
      mutate(Y = as.numeric(TWD97_Y.ori)) %>% 
      st_as_sf(., coords = c("X", "Y"), crs = 3826)
    
    if(is.na(x$TWD97_X)| is.na(x$TWD97_Y)){
      x$point_diff <- NA
    }else{
      P.set <- #表定位置
        x %>% 
        mutate(X = as.numeric(TWD97_X)) %>% 
        mutate(Y = as.numeric(TWD97_Y)) %>% 
        st_as_sf(., coords = c("X", "Y"), crs = 3826)
      
      x$point_diff <- 
        st_distance(P.sur, P.set,
                    by_element = TRUE) %>% 
        as.numeric()
    }
    x
   
    }) %>% 
  bind_rows() %>% 
  
  mutate(Macaca_sur.ori = Macaca_sur) %>% 
  mutate(Macaca_sur = ifelse(Macaca_sur %in% "1" & !is.na(Macaca_sur), 0,
                             ifelse(Macaca_sur %in% "2"& !is.na(Macaca_sur) , 1, Macaca_sur))) %>% 
  mutate(Macaca_sur = as.numeric(Macaca_sur))  
  
  

DF.3 <-  
  DF.2 %>% 
  mutate(analysis = ifelse(time_diff<6 & !is.na(time_diff),
                           paste0(analysis, ", 6min"), analysis)) %>% 
  mutate(analysis = ifelse(day_diff > 7, 
                           paste0(analysis, ", 7day"), analysis)) %>% 
  
  mutate(analysis = ifelse(as.numeric(Hour) >= 11, 
                           paste0(analysis, ", Toolate"), analysis))%>% 
  
  mutate(analysis = case_when(
             point_diff > 51 | is.na(point_diff) ~ paste0(analysis, ", locate"),  # range of gps =50m
             Point %in% "X" ~ paste0(analysis, ", locate"),  # range of gps =50m
             str_detect(備註, "刪除") ~ paste0(analysis, ", locate"),
             TRUE ~ analysis)) %>% 
  mutate(analysis = str_replace(analysis, "Y, ","")) 
         



 DF.3 %>%  #各林管處的各疏失情況的樣點數
   filter(!is.na(Macaca_sur) ) %>%
   group_by(Office) %>%
   summarise(
     FULL = n(),
     '6min' = str_subset(analysis,"6min") %>% length,
     '7day' = str_subset(analysis,"7day") %>% length,
     'Toolate' = str_subset(analysis,"Toolate") %>% length,
     'locate' = str_subset(analysis,"locate") %>% length,
     'good' = str_subset(analysis,"Y") %>% length
  ) 
 
 
 DF.2 %>%   #林管處無疏失情形的樣點數
   filter(!is.na(Macaca_sur) ) %>% 
   filter(analysis %in% "Y" ) %>% 
   group_by(Office, analysis, Survey) %>%
   summarise(N = n()) %>%
   reshape2::dcast( Office ~ analysis + Survey, guess.var = "N")
 

 
 
 DF.3 %>%    #時無疏失情形下，林管處在1、2旅次的猴群、孤猴數
   filter(analysis %in% "Y")  %>% 
   filter(!is.na(Macaca_sur) ) %>% 
   group_by(Office, Macaca_sur, Survey) %>% 
   summarise(N = n()) %>% 
   reshape2::dcast(Office + Survey ~ Macaca_sur, guess_value  = "N")


 DF.3 %>% 
   filter(analysis %in% "Y")  %>% 
   filter(!is.na(Macaca_sur)) %>% 
   group_by(Office, Macaca_sur, TypeName.1) %>% 
   summarise(N = n()) %>%
   reshape2::dcast( Office + Macaca_sur ~ TypeName.1, guess.var = "N")
 
 M.data <- 
   DF.3 %>% 
   relocate(ends_with("ori"), .after = last_col()) %>% 
   relocate(ends_with("diff"), .after = last_col())%>% 
   relocate("地點 (樣區名稱)", .after = "Station") %>% 
   setNames(., str_replace(colnames(.), "^地點.*", "Name")) %>% 
   setNames(., str_replace(colnames(.), "^join_TypeName$", "TypeName")) %>% 
   setNames(., str_replace(colnames(.), "^distance$", "Distance")) %>% 
   arrange(Office, Site_N, Survey, Point)
   



 #猴群、孤猴的統計資料             
 
 M.data  %>% 
   filter(!is.na(Macaca_sur) ) %>% 
   group_by(Office, Macaca_sur) %>% 
   summarise(N = n()) %>% 
   reshape2::dcast(Office ~ Macaca_sur, guess_value  = "N")
 
 
 NOTE <- data.frame(
   說明 = c(
     "1. 欄位名有.ori 為調查者所填的原始數字",
     "2. 欄位名有.diff 為輔助欄位，用於計算時間差及位置偏差 ",
     "本資料集為林務局獼猴調查資料的合併檔，並已完成清理動作。"
     ))
 
 
  write_xlsx( list('NOTE' = NOTE, 'Data' = M.data), "./data/clean/full_combind_Forestrydata_2021_V2.xlsx")
 
 #Part 3 可納入分析的資料----------------
#(僅留下 距離A、B、海拔50m以上、森林、300m的猴群)---------- 



  