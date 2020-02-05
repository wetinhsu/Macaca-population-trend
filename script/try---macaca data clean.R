library(tidyverse)
library(dplyr)
library(readxl)
library(openxlsx)


list.files() %>%
  lapply(.,function(x){
    getSheetNames(x)
    #read_excel(x,col_types = "text",  sheet = "獼猴調查結果")
  })



list.files() %>%
  
  lapply(.,function(x){
    read_excel(x,col_types = "text",  sheet = "獼猴調查結果")%>%
      select(林管處, 工作站, 樣區名稱, 樣區編號, 樣點編號,`X座標(TWD97)`, `Y座標(TWD97)`,
                年, 月, 日, 旅次, 時, 分,數量, 距離, 叫聲,`棲地類型(主要)`,
                調查者 , 備註)%>%
      
      filter(林管處 %in% c("羅東", "新竹", "東勢", "南投", "嘉義", "屏東", "花蓮", "臺東"))%>%
      fill(樣區名稱,  .direction ="down")%>% 
      fill(樣區編號,  .direction ="down") 
  })


add_column('說明' = "NA", .before = '林管處') %>%
  split(.$樣區名稱)%>%
  lapply(.,function(x){
    x%>%
      fill(年,  .direction ="down")%>%
      fill(月,  .direction ="down") %>%
      fill(日,  .direction ="down") %>%
      fill(旅次, .direction ="down")
  })
%>%
  mutate(年=ifelse(is.na(月),NA,"月"))



