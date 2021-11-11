#樣點資料檢核
#---- load library

library(tidyverse)
library(readxl)
library(writexl)

#--------------

DF<- 
  lapply(paste0("./data/raw/FORESTRYdata/", 2020:2021), function(x){
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


#調查者的統計資料
List_surveyor <- 
  DF %>%
  separate(.,Surveyor,
           into = paste0("Surveyor","_",0:10),
           sep ="、|,", extra = "drop", fill = "right") %>% 
  reshape2::melt(., id.vars = c("Office","Year", "Site_N", "Point",  "Survey"),
                 measure.vars = paste0("Surveyor","_",0:10),
                 variable.name = "Surveyor", value.name = "Name",)%>% 
  filter(!is.na(Name)) 

List_surveyor %>% 
  mutate(SP = paste0(Site_N, "-", Point)) %>% 
  group_by(Year, Office) %>% 
  summarise(Site_n = Site_N %>% unique %>% length,     #樣區數
            Person_n = Name %>% unique %>% length) %>%  #人數
  arrange(Office)

