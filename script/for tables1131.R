# for summary table

library(readxl)
library(data.table)
library(magrittr)
library(dplyr)
library(writexl)



S.all <- read_xlsx( "data/clean/data_for_analysis_1519.xlsx")



sum.table.0 <- S.all %>%     #total survey point
  .[ Do.survey %in% 1, ] %>%
  .[, .(data_N = length(unique(paste0(Site_N,Point)))), by = list(Year, Survey)] %>%
  dcast(., Year ~ Survey , value.var = c("data_N"))

sum.table.0.1 <- S.all %>%     #total survey point <100m for group
  .[ Do.survey %in% 1, ] %>%
  .[ Macaca_dist %in% c("A","B"), ] %>%
  .[ Macaca_sur %in% 1, ] %>%
  .[, .(data_N = length(unique(paste0(Site_N,Point)))), by = list(Year, Survey)] %>%
  dcast(., Year ~ Survey , value.var = c("data_N"))



sum.table.1<- S.all %>%     #total survey point ~ county  for group
  .[Macaca_sur %in% 1, ] %>%
  .[, .(data_N = length(unique(paste0(Site_N,Point,Survey))),
        site_N = length(unique(Site_N)),
        point_N = length(unique(paste0(Site_N,Point)))), by = list(Year, County)] %>%
  dcast(., County ~ Year, value.var = c("data_N", "site_N","point_N"))

sum.table.2<- S.all %>%       #total survey point ~ county for single 
  .[Macaca_sur %in% 0, ] %>%
  .[, .(data_N = length(unique(paste0(Site_N,Point,Survey))),
        site_N = length(unique(Site_N)),
        point_N = length(unique(paste0(Site_N,Point)))), by = list(Year, County)] %>%
  dcast(., County ~ Year, value.var = c("data_N", "site_N","point_N"))

sum.table.3<- S.all %>%    #survey point ~ TypeName  for all survey 
  .[ Do.survey %in% 1, ] %>%
  .[ Distance <20 , ] %>%
  .[ ! is.na(TypeName.1) , ] %>%
  .[, .(data_N = length(unique(paste0(Site_N,Point)))), by = list(Year, Survey, TypeName.1)] %>%
  dcast(., Year + Survey ~ TypeName.1, value.var = c("data_N"))

sum.table.4<- S.all %>%    #survey point ~ TypeName  for group
  .[ Do.survey %in% 1, ] %>%
  .[ Distance <20 , ] %>%
  .[ Macaca_sur %in% 1, ] %>%
  .[ ! is.na(TypeName.1) , ] %>%
  .[, .(data_N = length(unique(paste0(Site_N,Point)))), by = list(Year, Survey, TypeName.1)] %>%
  dcast(., Year + Survey ~ TypeName.1, value.var = c("data_N"))


sum.table.5<- S.all %>%    #survey point ~ TypeName + Altitude for all survey 
  .[ Do.survey %in% 1, ] %>%
  .[ Distance <20 , ] %>%
  .[ ! is.na(TypeName.1) , ] %>%
  .[, .(data_N = length(unique(paste0(Site_N,Point)))), by = list(Year, Survey, TypeName.1, Altitude)] %>%
  dcast(., Year + Survey ~  Altitude +TypeName.1, value.var = c("data_N"))


sum.table.6<- S.all %>%    #survey point ~ TypeName + Altitude for group 
  .[ Do.survey %in% 1, ] %>%
  .[ Distance <20 , ] %>%
  .[ Macaca_sur %in% 1, ] %>%
  .[ ! is.na(TypeName.1) , ] %>%
  .[, .(data_N = length(unique(paste0(Site_N,Point)))), by = list(Year, Survey, TypeName.1, Altitude)] %>%
  dcast(., Year + Survey ~  Altitude +TypeName.1, value.var = c("data_N"))




write_xlsx(list(  # 除了sum.table.0.1 之外，其餘的表格都排除沒距離沒時段，距離在A、B、C之內。
  "total.survey.point" = sum.table.0 ,
  "point.100m.group" = sum.table.0.1,
  "county.group" = sum.table.1, 
  "county.single " = sum.table.2,
  "TypeName.Allsurvey" = sum.table.3,
  "TypeName.group" = sum.table.4, 
  "TypeNameAltitude.Allsurvey" = sum.table.5,
  "TypeNameAltitude.group" = sum.table.6 
),  "Results/sum_table_1031.xlsx")
