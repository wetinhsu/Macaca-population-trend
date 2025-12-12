library(tidyverse)
library(sf)
library(here)
library(DBI)
library(RSQLite)
library(dbx)

con3 <-  dbConnect(RSQLite::SQLite(),
                   dbname="D:/R/test/Macaca-population-trend/data/raw/FORESTRYdata/data.db")

forest3 <- dbReadTable(con3, "forest3") 


con <-  dbConnect(RSQLite::SQLite(), dbname="D:/R/test/DB/P_BBS.db")

list_Point<- dbReadTable(con, "list_Point") 
table_Point<- dbReadTable(con, "table_Point")
list_Site<- dbReadTable(con, "list_Site")

dbDisconnect(con)  

need_update <- 
list_Point %>% 
  left_join(forest3, by = "PointID") %>% 
  filter(as.Date(tagert_recorded) < as.Date(recorded)|is.na(tagert_recorded)) 
#

source(file = here("script/Analysis 2025 for Forestry/forest polygon.R"))

update_forest <- 
need_update %>% 
  dplyr::select( PointID, X_97, Y_97) %>% 
  filter(!is.na(X_97)) %>% 
  st_as_sf(., coords = c("X_97", "Y_97"), crs = 3826) %>% 
  mutate(n_nest (.,  nc.b)) %>% 
  st_drop_geometry() %>% 
  left_join(need_update[,c('recorded', 'PointID')], by = "PointID") %>% 
  setNames(str_replace_all(colnames(.), "recorded", "tagert_recorded"))


# 
# dbxUpsert(con3, "forest3",
#           update_forest, where_cols=c("PointID"))



source(file = here("script/Analysis 2025 for Forestry/raster.R"))

update_Alt <- 
list_Point %>% 
  dplyr::select(PointID, X_97, Y_97) %>% 
  filter(!is.na(X_97)) %>% 
  st_as_sf(., coords = c("X_97", "Y_97"), crs = 3826) %>% 
  st_transform(crs(imported_raster)) %>%
  extract(imported_raster, ., method='simple') %>% 
  data.frame(Altitude = .) %>% 
  bind_cols(list_Point[!is.na(list_Point$X_97),],.)%>% 
  filter(!is.na(Altitude)) %>% 
  dplyr::select(PointID, Altitude) 

# dbxUpdate(con3, "forest3",
#           update_Alt, where_cols=c("PointID"))


update_Alt <- 
  need_update %>% 
  dplyr::select(PointID, X_97, Y_97) %>% 
  filter(!is.na(X_97)) %>% 
  bind_cols(  
  st_as_sf(., coords = c("X_97", "Y_97"), crs = 3826) %>% 
  st_transform(crs(imported_raster)) %>%
  extract(imported_raster, ., method='simple') %>% 
  data.frame(Altitude = .) 
  )%>% 
#  filter(!is.na(Altitude)) %>% 
  dplyr::select(PointID, Altitude) 

# 
# aa <- 
# update_forest %>% 
#   full_join(update_Alt, by = "PointID") %>% 
#   rows_upsert(forest3, .,by =c("PointID"))
# 
# 
# 
# DBI::dbWriteTable(con3,"forest3", aa, overwrite = TRUE)
# 

for (i in 1:nrow(update_forest)) {
  dbExecute(con3, 
  " UPDATE forest3
    SET TypeName = ?,
    Distance = ?
    WHERE PointID = ?",
            params = list(update_forest$TypeName[i],
                          update_forest$Distance[i],
                          update_forest$PointID[i])
  )
}

#還有upsert的問題


for (i in 1:nrow(update_Alt)) {
  dbExecute(con3, "
    UPDATE forest3
    SET Altitude = ?
    WHERE PointID = ?",
            params = list(update_Alt$Altitude[i], update_Alt$PointID[i])
  )
}





dbDisconnect(con3) 
