#樣點資料檢核
#---- load library

library(tidyverse)
library(readxl)
library(writexl)
library(sf)

#------------
path <-  "//10.40.1.138/Bird Research/BBSTW/15_計畫/臺灣獼猴族群監測計畫/與林務局合作監測/各林管處陳報的樣區和樣點/"
#path <-  "C:/Users/wetin/Desktop/R/"

M.Point <- read_excel(paste0(path,"樣區樣點資訊_2020.xlsx"),
                      sheet = "樣點")

M.Point.del <- read_excel(paste0(path,"樣區樣點資訊_2020.xlsx"),
                      sheet = "刪除樣區的樣點", cell_cols("A:M")) %>% 
  mutate(樣區樣點編號 = paste0("d", 樣區樣點編號))



st_M.Point <-
  M.Point %>% 
  rbind(., M.Point.del) %>% 
  mutate(X = as.numeric(TWD97_X)) %>% 
  mutate(Y = as.numeric(TWD97_Y)) %>%
  st_as_sf(., coords = c("X", "Y"), crs = 3826)




#--------------

S20<- 
lapply(paste0("./data/raw/FORESTRYdata/", 2020), function(x){
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
  mutate(Macaca_voice = gsub("n","N", .$Macaca_voice)) %>% 
  mutate(Macaca_voice = ifelse(Macaca_sur %in% 0 & Macaca_voice %in% c("N"), NA,  Macaca_voice)) %>% 
  
  mutate(Date = ISOdatetime(Year, Month, Day, Hour, Minute, sec = 0) ) %>% 
  mutate(Office = ordered(Office, c("羅東", "新竹", "東勢", "南投", "嘉義", "屏東", "花蓮", "臺東")))


S20 %>% 
  filter(Macaca_sur %in% 0) %>% 
  filter(! is.na(Macaca_dist) | !is.na(Macaca_voice) ) 

S20 %>% 
  filter( Macaca_sur %in% c(1, 2)) %>% 
  filter( is.na(Macaca_dist) | is.na(Macaca_voice) ) 

#Part 1 最粗的資料----
#(收到的資料)----------------------

#樣區樣點的統計資料

Count_Point_Survey <- 
S20 %>% 
  filter(!is.na(Macaca_sur) ) %>% 
  group_by(Office, Survey) %>% 
  summarise(Site_n = Site_N %>% unique %>% length,
            Data_n = n()) %>% 
  reshape2::melt(id = 1:2) %>% 
  reshape2::dcast(Office ~ Survey + variable, guess_var = "value")

#有些樣點兩旅次做在不同位置上，所以data數不會剛好是point的兩倍
#Data_n奇數筆原因是花蓮的長良林道兩旅次做的樣點數不一樣，第2旅次為8樣點，第1旅次7樣點。


#調查者的統計資料
List_surveyor <- 
S20 %>%
  separate(.,Surveyor,
           into = paste0("Surveyor","_",0:10),
           sep ="、|,", extra = "drop", fill = "right") %>% 
  reshape2::melt(., id.vars = c("Office","Year", "Site_N", "Point",  "Survey"),
                 measure.vars = paste0("Surveyor","_",0:10),
                 variable.name = "Surveyor", value.name = "Name",)%>% 
  filter(!is.na(Name)) 

List_surveyor %>% 
  mutate(SP = paste0(Site_N, "-", Point)) %>% 
  group_by(Office) %>% 
  summarise(Site_n = Site_N %>% unique %>% length,     #樣區數
            Person_n = Name %>% unique %>% length)  #人數

#猴群、孤猴的統計資料             

S20  %>% 
  mutate(SP = paste0(Site_N, "-", Point))  %>% 
  filter(!is.na(Macaca_sur) ) %>% 
  group_by(Office, Macaca_sur) %>% 
  summarise(N = SP %>% length) %>% 
  reshape2::dcast(Office ~ Macaca_sur, guess_value  = "N")

#Part 2 刪疏失的資料----------------
#(刪除不足6分鐘、 after11pm、不在檢核點上、同一旅次超過7日才完成調查)----------
#同一旅次同一樣區內超過7日才完成調查，整個旅次的資料方棄。----

 S20.2 <- 
   S20 %>% 
   add_column(analysis = "Y", .before = "Office") %>% 
   mutate(SS = paste0(Site_N, "-", Survey)) %>% 
   split(.,.$SS) %>% 
   
   lapply(., function(x){  #計算調查時間間隔
     x[order(x$Date),] %>%
       mutate(time2 = NA) %>% 
       mutate(time2 = c(Date[2: length(Date)], NA)) %>%
       mutate(time.diff = difftime(time2, Date, units ="mins")%>% as.numeric()) %>% #時間相隔，以分為單位
       mutate(day.diff = difftime(time2, Date, units ="day"))  #時間相隔，以日為單位
     }) %>%   


   lapply(., function(x){  #計算位置誤差距離：依樣區旅次分別計算各樣區內，各樣點離做近檢核點的距離
     
     st_M.Point <-  #與st_S20對應的樣區
       st_M.Point %>% 
       filter(Macaca_Site %in% unique(x$Site_N)) 
     
     st_S20.2 <-   
       x %>% 
       mutate(SP = ifelse(nchar(Point) %in% 1,
                          paste0(Site_N, "-0", Point),
                          paste0(Site_N, "-", Point))) %>%
       
       mutate(X = as.numeric(TWD97_X)) %>% 
       mutate(Y = as.numeric(TWD97_Y)) %>% 
       st_as_sf(., coords = c("X", "Y"), crs = 3826)
     
     st_S20.2$point_O <-
       st_distance(st_S20.2, st_M.Point) %>% 
       apply(.,1,which.min) %>% 
       st_M.Point$樣區樣點編號[.] 
     
     st_S20.2$point.diff <- 
       st_distance(st_S20.2, st_M.Point) %>% 
       apply(., 1,min) 
     
     return(st_drop_geometry(st_S20.2))}) %>% 
   
   lapply(., function(x){     # 超過7日才完成調查，整個旅次的資料方棄
     if(TRUE %in% (as.numeric(x$day.diff)>8)) x$analysis <- "N"
     return(x)
   })%>%  
   
    bind_rows() %>% 
  select(-SS) %>%   #刪除輔註欄位
  
  mutate(analysis = ifelse(as.numeric(Hour) >= 11, "N1", analysis)) %>%  #11點
  mutate(analysis = ifelse(!analysis %in% "Y", analysis,
                           ifelse(time.diff >= 6 | is.na(time.diff), analysis, "N2"))) %>%   #6分鐘
  mutate(analysis = ifelse(!analysis %in% "Y", analysis,
                           ifelse(point.diff > 50 | Point %in% "X", "N3", analysis)))    # range of gps =50m



  S20.2 %>% 
    split(., row(.)) %>% 
    lapply(.,function(x){is.logical(x$SP ==  x$point_0)}) %>% do.call(rbind, .) 



 S20.2 %>% 
  group_by(Office, analysis) %>%
  summarise(N = n()) %>%
  reshape2::dcast( analysis ~ Office, guess.var = "N")
 
 
 S20.2 %>% 
   filter(!is.na(Macaca_sur) ) %>% 
   filter(analysis %in% "Y" ) %>% 
   group_by(Office, analysis, Survey) %>%
   summarise(N = n()) %>%
   reshape2::dcast( Office ~ analysis + Survey, guess.var = "N")
 

 
 
 S20.2 %>% 
   filter(analysis %in% "Y")  %>% 
   filter(!is.na(Macaca_sur) ) %>% 
   group_by(Office, Macaca_sur, Survey) %>% 
   summarise(N = n()) %>% 
   reshape2::dcast(Office + Survey ~ Macaca_sur, guess_value  = "N")


 S20.2 %>% 
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
   S20.2 %>% 
   filter(analysis %in% "Y")  %>% 
   filter(!is.na(Macaca_sur) ) %>% 
   left_join(st_drop_geometry(st_M.Point), by = c("point_O" = "樣區樣點編號") ) %>% 
   select(analysis, Office, Station, 
          "Site_N" = Macaca_Site,
          "Point" = 樣點代號, 
          "X" = TWD97_X.y,
          "Y" = TWD97_Y.y,
          Year, Month, Day, Survey, Macaca_sur,
          Hour, Minute, Day, Date,
          Macaca_dist,
          "Distance" = distance, 
          "TypeName" = join_TypeName, TypeName.1, Altitude)
 
 
  write_xlsx( M.data, "./data/clean/full_combind_Forestrydata_V1.xlsx")
 
 #Part 3 可納入分析的資料----------------
#(僅留下 距離A、B、海拔50m以上、森林、300m的猴群)---------- 


 

 