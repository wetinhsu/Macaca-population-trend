library(tidyverse)
library(readxl)
library(here)
library(DBI)
library(RSQLite)

con <-  dbConnect(RSQLite::SQLite(), dbname="../DB/P_BBS.db")
list_Site<- dbReadTable(con, "list_Site")


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
  
  filter(str_detect(analysis ,"^Y") )

df <- 
M.data %>% 
  dplyr::select(Year, Office, Station, Site_N, Point, 
         TWD97_X, TWD97_Y, Macaca_sur.ori) %>% 
  unique() 


df.1 <- 
df%>% 
  dplyr::select(Site_N, Point, TWD97_X, TWD97_Y)%>% 
  unique() %>% 
  mutate(Site_N = ifelse(Site_N == "MB-C11-10", "MA-C11-10", Site_N)) %>% 
  right_join( list_Site[,c('樣區名稱','獼猴樣區編號')],.,
            by = c('獼猴樣區編號' = 'Site_N'))

df.1 %>% setNames(.,c('樣區名稱',"樣區編號","樣點代號","TWD97_X","TWD97_Y")) %>% 
write.csv(., "./Report of Foresty_20251127/20260113/座標.csv",
          fileEncoding = "big5", row.names = F)


df%>% 
  filter(Macaca_sur.ori == 2) %>% 
  dplyr::select(Site_N, Point, TWD97_X, TWD97_Y)%>% 
  unique() %>% 
  mutate(Site_N = ifelse(Site_N == "MB-C11-10", "MA-C11-10", Site_N)) %>% 
  right_join( list_Site[,c('樣區名稱','獼猴樣區編號')],.,
              by = c('獼猴樣區編號' = 'Site_N'))%>% setNames(.,c('樣區名稱',"樣區編號","樣點代號","TWD97_X","TWD97_Y")) %>% 
  write.csv(., "./Report of Foresty_20251127/20260113/猴群座標.csv",
            fileEncoding = "big5", row.names = F)

df%>% 
  filter(Macaca_sur.ori == 2) %>% 
  dplyr::select(Year,Site_N, Point, TWD97_X, TWD97_Y)%>% 
  unique() %>% 
  mutate(Site_N = ifelse(Site_N == "MB-C11-10", "MA-C11-10", Site_N)) %>% 
  right_join( list_Site[,c('樣區名稱','獼猴樣區編號')],.,
              by = c('獼猴樣區編號' = 'Site_N'))%>%
  filter(Year == 2025) %>% 
  dplyr::select(Year,樣區名稱,獼猴樣區編號, Point, TWD97_X, TWD97_Y)%>% 
  setNames(.,c('年','樣區名稱',"樣區編號","樣點代號","TWD97_X","TWD97_Y")) %>% 
  write.csv(., "./Report of Foresty_20251127/20260113/2025猴群座標.csv",
            fileEncoding = "big5", row.names = F)

df%>% 
  filter(Macaca_sur.ori == 2) %>% 
  dplyr::select(Year,Site_N, Point, TWD97_X, TWD97_Y)%>% 
  unique() %>% 
  mutate(Site_N = ifelse(Site_N == "MB-C11-10", "MA-C11-10", Site_N)) %>% 
group_by(Year) %>% 
  summarise(n = n())

#-海拔50m------------------------------------------


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
  
  filter(analysis =="Y") 

df <- 
  M.data %>% 
  dplyr::select(Year, Office, Station, Site_N, Point, 
                TWD97_X, TWD97_Y, Macaca_sur.ori) %>% 
  unique() 


df.1 <- 
  df%>% 
  dplyr::select(Site_N, Point, TWD97_X, TWD97_Y)%>% 
  unique() %>% 
  mutate(Site_N = ifelse(Site_N == "MB-C11-10", "MA-C11-10", Site_N)) %>% 
  right_join( list_Site[,c('樣區名稱','獼猴樣區編號')],.,
              by = c('獼猴樣區編號' = 'Site_N'))

df.1 %>% setNames(.,c('樣區名稱',"樣區編號","樣點代號","TWD97_X","TWD97_Y")) %>% 
  write.csv(., "./Report of Foresty_20251127/20260113/50m_Forest/座標.csv",
            fileEncoding = "big5", row.names = F)


df%>% 
  filter(Macaca_sur.ori == 2) %>% 
  dplyr::select(Site_N, Point, TWD97_X, TWD97_Y)%>% 
  unique() %>% 
  mutate(Site_N = ifelse(Site_N == "MB-C11-10", "MA-C11-10", Site_N)) %>% 
  right_join( list_Site[,c('樣區名稱','獼猴樣區編號')],.,
              by = c('獼猴樣區編號' = 'Site_N'))%>% setNames(.,c('樣區名稱',"樣區編號","樣點代號","TWD97_X","TWD97_Y")) %>% 
  write.csv(., "./Report of Foresty_20251127/20260113/50m_Forest/2020-2025猴群座標.csv",
            fileEncoding = "big5", row.names = F)

df%>% 
  filter(Macaca_sur.ori == 2) %>% 
  dplyr::select(Year,Site_N, Point, TWD97_X, TWD97_Y)%>% 
  unique() %>% 
  mutate(Site_N = ifelse(Site_N == "MB-C11-10", "MA-C11-10", Site_N)) %>% 
  right_join( list_Site[,c('樣區名稱','獼猴樣區編號')],.,
              by = c('獼猴樣區編號' = 'Site_N'))%>%
  filter(Year == 2025) %>% 
  dplyr::select(Year,樣區名稱,獼猴樣區編號, Point, TWD97_X, TWD97_Y)%>% 
  setNames(.,c('年','樣區名稱',"樣區編號","樣點代號","TWD97_X","TWD97_Y")) %>% 
  write.csv(., "./Report of Foresty_20251127/20260113/50m_Forest/2025猴群座標.csv",
            fileEncoding = "big5", row.names = F)

df%>% 
  filter(Macaca_sur.ori == 2) %>% 
  dplyr::select(Year,Site_N, Point, TWD97_X, TWD97_Y)%>% 
  unique() %>% 
  mutate(Site_N = ifelse(Site_N == "MB-C11-10", "MA-C11-10", Site_N)) %>% 
  group_by(Year) %>% 
  summarise(n = n())
