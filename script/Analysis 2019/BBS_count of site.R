#---- load library
library(openxlsx)
library(readxl)
library(writexl)
library(tidyverse)
library(sf)
library(ggspatial)
library(ggrepel)
library(extrafont)
#-------------
setwd("C:/Users/user/Desktop/tmp/bbs_handover_temp_v20190123 - WT")


dfo  <- read_xlsx("dfs2.xlsx",
                  #      range = cell_cols("AB:AE"),
                  col_types = "text") 
summary(dfo )

site <- read.xlsx("./sampling/樣區表_v2.7.xlsx", sheet=c("樣區表"), startRow=1, colNames=TRUE)
colnames(site)
site <- site %>% select(plotnr,HJHsiu3,ELEV,
                        樣區編號, `地點.(樣區名稱)`, `X_wgs84`, `Y_wgs84`)
colnames(site) <- c("plotnr", "eco3", "elev3", "siteid", "sitename","X_wgs84", "Y_wgs84")

# 建立Birdstats的cov1 --- 5: >2500m, 4: 1000-2500m, 3: West(<1000m), 2: East(<1000m), 1: North(<1000m)

site$cov1 <- ifelse(site$elev == 3, 5, 
                    ifelse(site$elev == 2, 4, 
                           ifelse(site$eco3 == 'West', 3, 
                                  ifelse(site$eco3 == 'East', 2, 
                                         ifelse(site$eco3 == 'North', 1,
                                                ifelse(site$eco3 == 'Lanyu', 6,
                                                       7))))))
#----------
(df <- 
dfo %>% 
  filter(!vernacularName %in% grep("屬$|科$|目$|綱$|^XX|山羌|獼猴$|松鼠$|食蟹獴|黃喉貂|蛙$|NN|山羊$|無法調查", .$vernacularName, value = TRUE)) %>% 
  
  select(year = 年,
         siteid = 樣區編號,
         pointid = 樣點編號) %>% 
  unique() %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(stage = ifelse(year %in% c(2009:2010),
                        "2009-2010",ifelse(year %in% 2011,
                                           "2011",
                                           ifelse(year %in% c(2012:2017),
                                                  "2012-2017", "2018-2019")))) %>% 
  group_by(year, stage) %>% 
  summarise(P.N = length(pointid), S.N = length(unique(siteid))) )
  
  
df%>% 
  group_by(stage) %>% 
  summarise(mean.s=mean(S.N, na.rm = T),
            min.s=min(S.N, na.rm = T),
            max.s=max(S.N, na.rm = T),
            mean.p=mean(P.N, na.rm = T),
            min.p=min(P.N, na.rm = T),
            max.p=max(P.N, na.rm = T)
            ) %>% 
  as.data.frame() %>% 
  write_xlsx(., "BBS_count of site.xlsx")


