library(sf)
library(tidyverse)
library(readxl)
library(here)


colname_en <- c('Office','Station','Site_N','Name',
                'Year','Month','Day',
                'Survey','Surveyor','Point',
                'TWD97_X','TWD97_Y','Hour','Minute',
                'Macaca_sur','Macaca_dist','Macaca_voice','Habitat', 'Note')

colname_ch <- c('分署','工作站','樣區編號','樣區名稱',
                '年','月','日',
                '旅次','調查者','樣點編號',
                'TWD97_X','TWD97_Y','時','分',
                '數量','距離','叫聲','棲地類型\\(主要\\)','備註')

M.data <- 
  list.files(here("./data/clean/Forestry/for analysis/"),
             full.names = T,pattern = "xlsx$|xls$") %>% 
  lapply(., read_excel, sheet="Data", col_types = "text") %>% 
  bind_rows() %>% 
  
  mutate(Office = str_replace_all(Office, c("羅東" = "宜蘭",
                                            "東勢" = "臺中"))) %>%
  
  # mutate(Office = 
  #          ordered(Office,
  #                  levels = c("羅東", "新竹", "東勢", "南投",
  #                             "嘉義", "屏東", "花蓮", "臺東"),
  #                  labels = c("Luodong", "Hsinchu", "Dougshih", "Nantou",
  #                             "Chiayi", "Pingtung", "Hualien", "Taitung")
  #                  )
  #        ) %>% 
  
  mutate_at(c("Year", "Survey","Month",
              "Day", "Macaca_sur", "Distance", "julian.D", "Altitude"), as.numeric) %>% 
  
  mutate(TypeName.1 = case_when(
    TypeName.1 %in% "闊葉林" ~ "broad_leaved",
    TypeName.1 %in% "針葉林" ~ "coniferous",
    TypeName.1 %in% "竹林" ~ "Bamboo",
    TypeName.1 %in% "混淆林" ~ "mixed",
    TypeName.1 %in% "非森林" ~ "Not forest"
  )) %>% 
  
  filter(analysis %in% "Y")

df <- 
M.data %>% 
  dplyr::select(`Office`:`Macaca_dist`, TWD97_X, TWD97_Y, Macaca_sur.ori) %>% 
  dplyr::select(-Macaca_sur) %>% 
  setNames(str_replace_all(names(.),"Macaca_sur.ori", "Macaca_sur")) 


df %>% 
  filter(Macaca_sur == "2") %>% 
  dplyr::select(`Site_N`, `Name`,Point, TWD97_X, TWD97_Y, Year)%>% 
  unique() %>% 
  filter(Year %in% c(2025)) %>% 
  setNames(str_replace_all(names(.),setNames(colname_ch,colname_en)))  %>%
  write.csv(., "D:/R/test/Macaca-population-trend/Report of Foresty_20251127/gis plot_20260113/2025猴群座標.csv",
            fileEncoding = "big5", row.names = F)

df %>% 
  filter(Macaca_sur == "2") %>% 
  dplyr::select(`Site_N`, `Name`,Point, TWD97_X, TWD97_Y)%>% 
  unique() %>% 
  setNames(str_replace_all(names(.),setNames(colname_ch,colname_en)))  %>%
  write.csv(., "D:/R/test/Macaca-population-trend/Report of Foresty_20251127/gis plot_20260113/2020-2025猴群座標.csv",
            fileEncoding = "big5", row.names = F)


#全樣點
df %>% 
  dplyr::select(`Site_N`, `Name`,Point, TWD97_X, TWD97_Y)%>% 
  unique() %>% 
  setNames(str_replace_all(names(.),setNames(colname_ch,colname_en))) %>%
  write.csv(., "D:/R/test/Macaca-population-trend/Report of Foresty_20251127/gis plot_20260113/獼猴調查位置.csv",
            fileEncoding = "big5", row.names = F)
