# data analysis

library(data.table)
library(magrittr)
library(multcomp)
library(ggplot2)
library(rtrim)
library(dplyr)
library(openxlsx)
library(rtrim)

#------------------------------
setwd("D:/R/bbs_handover_temp_v20190123 - WT")
###### 建立Birdstats需要的樣點代號 plotnr
site <- read.xlsx("./sampling/02_樣區表_v2.7.xlsx", sheet=c("管理者用"), startRow=1, colNames=TRUE)
colnames(site)
site <- site[,c("plotnr","HJHsiu3","ELEV","樣區編號", "地點.(樣區名稱)")]
colnames(site) <- c("plotnr", "eco3", "elev3", "siteid", "sitename")

# 建立Birdstats的cov1 --- 5: >2500m, 4: 1000-2500m, 3: West(<1000m), 2: East(<1000m), 1: North(<1000m)

site$cov1 <- ifelse(site$elev == 3, 5, 
                    ifelse(site$elev == 2, 4, 
                           ifelse(site$eco3 == 'West', 3, 
                                  ifelse(site$eco3 == 'East', 2, 
                                         ifelse(site$eco3 == 'North', 1,
                                                ifelse(site$eco3 == 'Lanyu', 6,
                                                       7))))))

site <- as.data.table(site)

# check for rows with NA
site[!complete.cases(site),]

# if NA kick it out
site <- site[ !(plotnr %in% NA)]

# add it back to all data
colnames(site)[4] <- "Site_N"
df <- left_join(df, site[,c(4,6)], by = "Site_N")
df <- as.data.table(df)

# check for NAs in cov1
df[!complete.cases(df$cov1),]


#----------------------------------------------------
#df請搭配20191210-GLMM的df使用。


#weights ===============================================
weight.o <- read_excel("./data/weight.xlsx")  %>% setDT  
colnames(weight.o) <- c("Name", "cov1", "Area", "Site_n","prob_Site", "prob_Area",  "weight")


weight <- df %>%
  .[, list(Year, Site_N, Point, Survey, cov1)] %>%
  .[!duplicated(.)] %>%
  .[, site_n :=length(unique(Site_N)), by = c( "cov1")]  %>%
  .[, point_n :=length(Point), by = c("Year", "Site_N", "cov1")]   %>%
  left_join(weight.o[, c(1:3, 6)])  %>% setDT %>%
  .[, weight := (prob_Area / site_n /point_n)] %>%  #weight := prob_Area / site_n /調查次數
  .[, list(Year, Site_N, cov1, weight)]%>%
  .[!duplicated(.)] 

df %<>%.[, species_nr :=840]
#region=========
#計算各物種在不同分區內的年平均樣區數，小於1者，設為NA，不列入分析

region <- df %>%
  .[, list(species_nr, Year, Site_N,  cov1)] %>%
  .[!duplicated(.)] %>%
  .[, cov1 := factor(cov1,1:7)] %>%
  
  split(., .$species_nr) %>%  #依species_nrz分成list的形式處理資料
  lapply(., function(x){      #計算出現各cov1的年平均樣區數，ex: 2011、2013有調查到，則算2011至今的年平均樣區數
    x %>% .[, year := factor(Year, min(Year) : max(df$Year))] %>%
      dcast(.,  year ~ cov1, value.var = "cov1", length, drop =F)  %>%
      .[,-1] %>%                  
      apply(., 2,mean)
  }) %>%
  do.call(rbind, .) %>%
  melt(.,value.name = "site_peryear") %>%  setDT %>%
  .[,list( species_nr = Var1,
           cov1 = Var2,
           site_peryear)] %>%
  .[site_peryear >= 1, Region := cov1  ] %>%
  .[cov1 %in% 5, Region := 4  ] %>%
  .[site_peryear < 1, Region := NA  ] 

summary(region)
#------------------------


bird.data <- df %>% setDT%>% 
  aggregate(Macaca_sur ~ Year + Site_N + species_nr, ., sum, na.rm=T, drop = T) %>% setDT %>%
  split(., .$species_nr)%>% 
  lapply(., function(x) left_join( weight, x, suffix = c("", ".y")) ) %>%
  mapply(   function(x, y) { left_join(x, y, by= "cov1", suffix = c("", ".y"))}, ., split(region,region$species_nr),SIMPLIFY =F) %>%
  #lapply(., function(x) left_join(x, habitat, suffix = c("", ".y")) )   %>% 
  lapply(., function(x) { x %>% setDT %>%
      .[, species_nr := unique(na.exclude(species_nr))] %>%
      .[order(Site_N),] %>%
      .[, n:= sum(Macaca_sur, na.rm = T), by = "Site_N"] %>%
      .[! (n %in% 0),] %>%    #移除沒有出現過鳥資料的siteid
      .[is.na(Macaca_sur), Macaca_sur:=0] %>%
      #.[!(Habitat %in% "A"), Habitat:= "others"] %>%
      .[order(Year),] %>%
      .[!is.na(Macaca_sur), min.y := min(Year)] %>%
      .[, min.y := min(min.y, na.rm = T)] %>%
      .[!(Year < min.y),] %>%                             #移除鳥類出現年份之前的資料
      .[, c( "species_nr.y", "site_peryear", "n", "min.y") := NULL]  %>% #移除輔助欄
      .[, Region := factor(Region)] #%>%
      #.[, Habitat := factor(Habitat)]
  } ) %>% 
  rbindlist(.)


setDF(bird.data)
m1 <- trim(bird.data,
           count_col = "Macaca_sur",
           site_col = "Site_N",
           year_col = "Year",
           weights_col = "weight",
           covar_cols = "Region",
           model =  2,
           #changepoints = "all",
           overdisp = T,
           serialcor = F, 
           autodelete = T, 
           stepwise = F)



summary(m1)
wald(m1)
totals(m1, "fitted", obs =F) %>% plot
index(m1, "fitted", covars = T) %>% plot
overall(m1,"imputed") %T>% plot


plot(overall(m1, "imputed"))
title( "imputed" ) 

heatmap(m1, "imputed") 
title( "imputed" )


index(m1, "imputed", covars = F) %>% plot(., pct = T, main = "imputed") 
index(m1, "imputed", covars = T) %>% plot(., pct = T, main = "imputed")
