#上傳TBN的資料格式
### 林保署要求國有林班地的獼猴與繁殖鳥資料上傳到TBN，所以TBN的RA上傳


library(tidyverse)
library(openxlsx)
library(DBI)
library(readxl)
library(here)
library(sf)

#Monkey---------------------------------
M.data <- 
  read_excel("./data/clean/Forestry/for analysis/for analysis Forestrydata_2024_V2.xlsx",
sheet="Data", col_types = "text")

  
  M.data %>% names() 
  
 DF <-  
  M.data%>% 
    filter(Year == 2024) %>% 
   filter(str_detect(analysis, '^Y')) %>% 
   filter(Macaca_sur == 1) %>% 
   select(analysis:Macaca_sur,TWD97_X, TWD97_Y) %>% 
   select(-analysis,
          -Office,-Station
   )  %>% 
   mutate(原始紀錄物種 = "臺灣獼猴",
          觀測方式 = "人為觀測",
          原始空間參考系統 = "TWD97",
          數量單位 = "群") %>% 
   mutate(Surveyor = str_replace_all(Surveyor, "、", ";")) %>% 
   mutate_at(c('Year', "Month", "Day", "Hour", "Minute"), as.integer) %>%
   
   mutate_at(c("TWD97_X", "TWD97_Y"), as.numeric) %>% 
   st_as_sf(coords = c("TWD97_X", "TWD97_Y"), crs = 3826, remove = F ) %>% 
   st_transform(4326) %>% 
   bind_cols( st_coordinates(.) )%>% 
   st_drop_geometry(.) %>% 
   mutate_at(c("TWD97_X", "TWD97_Y"), as.character) %>% 


   
   select(
     原始紀錄物種,
     數量 = Macaca_sur,
     數量單位,
     觀測年 = Year,
     觀測月 = Month,
     觀測日 = Day,
     觀測時 = Hour,
     觀測分 = Minute,
     原始X座標 = TWD97_X,
     原始Y座標 = TWD97_Y,
     '經度(十進位)' = X,
     '緯度(十進位)' = Y,
     原始空間參考系統,
     樣區代碼 = Site_N,
     樣點代號 = Point,
     地點 = Name,
     記錄者 = Surveyor,
     觀測方式
   ) %>% 
   mutate(nid = NA, .before = "原始紀錄物種") 
 #nid是RA的必要欄位，新資料此欄為NA
   
 

 
 write.xlsx(DF, "./上傳到TBN/Monkey2024_20250123.xlsx")

 #Bird---------------------------------
 path <- "D:/R/test/Foresty_clean/Foresty_clean/"
 
 
 df_criterion <- read_excel(paste0(path,"data/row/2024_data0_bird - check.xlsx"),
                            sheet = 1) %>% 
   filter(!林業署編號 %in% 'A21-04') 
 
 DF <- read_excel(paste0(path,"data/row/2024_data0_bird - check.xlsx"),
                  sheet = 2) %>% 
   mutate(time = ISOdatetime(年,月,日,時,分,0)) %>% 
   mutate(調查者 = str_replace_all(調查者, "/|\\.", "、"))%>% 
   
   filter(str_detect(鳥種檢核, "不使用", negate = T )|is.na(鳥種檢核))
 
 
 station_codes <- 
   read_excel("D:/R/test/Macaca-population-trend/data/raw/FORESTRYdata/2024/林業署獼猴調查樣區列表.xlsx",
              sheet = 1) %>% 
   select(Office:Site_N) 
 
 
 df <- 
   DF %>%
   mutate(鳥種 = case_when(
     str_detect(鳥種檢核, "\\(.*無法確認\\)") ~ "其他",
     str_detect(鳥種, "無|沒有鳥") ~ NA,
     TRUE ~ 鳥種
   )) %>% 
   mutate(數量 = str_replace_all(數量, "NA|na", NA_character_))%>% 
   mutate(結群 = str_replace_all(結群, "N", NA_character_)) %>% 
   mutate(鳥種 = str_replace_all(鳥種,
                               c("^畫眉" = "臺灣畫眉",
                                 "^火冠戴菊$" = "火冠戴菊鳥",
                                 "酒紅朱雀" = "臺灣朱雀",
                                 "深山竹雞" = "臺灣山鷓鴣",
                                 "虎鶇|白氏地鶇|虎斑地鶇" = "白氏地鶇 /虎斑地鶇",
                                 "^黃鶺鴒$|東方黃鶺鴒|西方黃鶺鴒" = "西方黃鶺鴒 / 東方黃鶺鴒",
                                 "台" = "臺",
                                 "橿鳥" = "松鴉",
                                 "小彎嘴畫眉" = "小彎嘴",
                                 "^灰喉山椒$" = "灰喉山椒鳥",
                                 "^竹雞" = "臺灣竹雞",
                                 "^筒鳥|^中杜鵑$" = "北方中杜鵑",
                                 "^藪鳥" = "黃胸藪眉",
                                 "綠繡眼" = "斯氏繡眼",
                                 "頭屋綠|頭屋線" = "頭烏線",
                                 "^毛腳燕$" = "東方毛腳燕",
                                 "^帝雉$" = "黑長尾雉",
                                 "^竹鳥$" = "棕噪眉",
                                 "^雨燕$" = "小雨燕",
                                 "鱗胸鷦鷯" = "臺灣鷦眉",
                                 "^白喉笑鶇$|^白喉噪眉$" = "臺灣白喉噪眉",
                                 "^紫嘯鶇$" = "臺灣紫嘯鶇",
                                 "金翼白眉" = "臺灣噪眉"))) 

 df_criterion_count <- 
   df %>% 
   full_join(df_criterion, ., by = c("林業署編號" = "Site_N"), keep =T) %>%
   
   mutate(資料分級 = 
            ifelse(str_detect(檢核結果, "OK"), "優",
                   ifelse(str_detect(檢核結果, "有疑慮"),"有疑慮", "待加強")),
          .before = '檢核結果') %>% 
   
   mutate(檢核結果 = str_remove_all( 檢核結果, "OK；|^OK|X；|^有疑慮；|待加強；")) %>% 
   
   mutate(NO = (as.numeric(row.names(.))+1)%/%2 , .before = '資料分級') %>% 

   filter(str_detect(鳥種, "沒有|猴|XX|其他", negate = T))   #全部的鳥紀錄
 
 df_OK <- 
 df_criterion_count %>% 
   filter(資料分級 == "優") %>% 
   filter(!(Site_N %in% 'MA-H32-06'& Survey %in%1) )%>% 
   
   #先刪除有疑問且未更正鳥種
   filter(str_detect(鳥種, "沒有|猴|XX|其他|屬|科|xx", negate = T)) %>% 
   filter(!is.na(數量)) %>% 
   
   mutate(數量 = 數量 %>%  str_remove(.,"^>")) %>% 
   separate(數量, into = paste0("n",1:4), sep = "-|\\t|\\s",
            remove = F, fill = "right")  %>%
   select(-`n2`,-`n3`,-`n4`) %>% 
   setNames(str_replace(colnames(.), "^n1", "number")) 

 df.1 <-  
 df_OK %>% 
   select(Site_N,樣區名稱,鳥種,number, Point,
          TWD97_X, TWD97_Y,
          調查者,Survey,時:日)%>% 
   mutate(觀測方式 = "人為觀測",
          原始空間參考系統 = "TWD97",
          數量單位 = "隻") %>% 
   mutate(原始空間參考系統 = 
            ifelse(TWD97_X<130, "WGS84", 原始空間參考系統))%>% 
   mutate(調查者 = str_replace_all(調查者, "、", ";")) %>% 
   
   
   bind_cols(
     df_OK %>% 
   st_as_sf(coords = c("TWD97_X", "TWD97_Y"), crs = 3826, remove = F ) %>% 
   st_transform(4326) %>% 
   st_coordinates(.)%>% 
   st_drop_geometry(.) %>% 
     as.data.frame() %>% 
     mutate(X = ifelse(X<100|Y<20, NA, X),
            Y = ifelse(X<100|Y<20, NA, Y)     )
   
   ) %>% 
   
   select(
     原始紀錄物種 = 鳥種,
     數量 = number,
     數量單位,
     觀測年 = 年,
     觀測月 = 月,
     觀測日 = 日,
     觀測時 = 時,
     觀測分 = 分,
     原始X座標 = TWD97_X,
     原始Y座標 = TWD97_Y,
     '經度(十進位)' = X,
     '緯度(十進位)' = Y,
     原始空間參考系統,
     樣區代碼 = Site_N,
     樣點代號 = Point,
     地點 = 樣區名稱,
     記錄者 = 調查者,
     觀測方式
   ) %>% 
   mutate(nid = NA, .before = "原始紀錄物種") 
 
 write.xlsx(df.1, "./上傳到TBN/Bird2024_20250124.xlsx")
 