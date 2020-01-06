# data analysis

library(data.table)
library(magrittr)
library(multcomp)
library(ggplot2)
library(rtrim)
library(dplyr)
library(openxlsx)
library(rtrim)
#--------------------------------

#Original data---- 

M.data <- read_excel("./data/clean/for analysis.xlsx",
                     sheet=1) %>% setDT %>% 
  .[, DATE := as.IDate(paste(Year, Month, Day, sep = "/"))] %>% 
  .[TypeName %like% "混", TypeName.n := "mixed"] %>% 
  .[TypeName %like% "竹林", TypeName.n := "Bamboo"] %>% 
  .[TypeName %like% "闊葉", TypeName.n := "broad-leaved"] %>% 
  .[TypeName %like% "針葉", TypeName.n := "coniferous"] %>% 
  .[, TypeName.1 := ifelse(Distance>20, "Not forest", TypeName.n)] %>% 
  .[, County := ordered(County,
                        c("宜蘭縣","基隆市","台北市","臺北市",
                          "新北市","台北縣","臺北縣",
                          "桃園縣","桃園市","新竹市",
                          "新竹縣","苗栗縣",
                          "台中市","臺中市",
                          "台中縣","臺中縣",
                          "彰化縣","南投縣","南投市",
                          "雲林縣","嘉義縣","嘉義市",
                          "台南市","臺南市",
                          "台南縣","臺南縣",
                          "高雄縣","高雄市",
                          "屏東縣", "花蓮縣",
                          "台東縣","臺東縣"))] %>% 
  
  .[County %in% list("宜蘭縣","基隆市","台北市","臺北市",
                     "新北市","台北縣","臺北縣",
                     "桃園縣","桃園市","新竹市",
                     "新竹縣","苗栗縣"), Region := "North"] %>%
  .[County %in% list("台中市","臺中市",
                     "台中縣","臺中縣",
                     "彰化縣","南投縣","南投市",
                     "雲林縣","嘉義縣","嘉義市"), Region := "Center"] %>%
  .[County %in% list("台南市","臺南市",
                     "台南縣","臺南縣",
                     "高雄縣","高雄市",
                     "屏東縣"), Region := "South"]%>%
  .[County %in% list("花蓮縣",
                     "台東縣","臺東縣"), Region := "East"] %>% 
  .[, julian.D := yday(DATE)] %>% 
  .[, Altitude_c := substr(Site_N,1,1)] %>% setDT 

M.data$Year %<>% as.numeric
M.data$Survey %<>% as.numeric
M.data$Point %<>% as.numeric
M.data$Macaca_sur %<>% as.numeric
M.data$Month %<>% as.numeric
M.data$Day %<>% as.numeric
M.data$Distance %<>% as.numeric
M.data$julian.D %<>% as.numeric
M.data$Region %<>% as.factor
M.data$TypeName.1 %<>% as.factor
M.data$Site_N %<>% as.factor



county.area <- read.csv("./data/clean/gis/county area.csv", header = T) %>% 
  setDT %>% 
  setnames(.,c("County", "Area", "perimeter")) %>% 
  .[County %in% list("宜蘭縣","基隆市","台北市","臺北市",
                     "新北市","台北縣","臺北縣",
                     "桃園縣","桃園市","新竹市",
                     "新竹縣","苗栗縣"), Region := "North"] %>%
  .[County %in% list("台中市","臺中市",
                     "台中縣","臺中縣",
                     "彰化縣","南投縣","南投市",
                     "雲林縣","嘉義縣","嘉義市"), Region := "Center"] %>%
  .[County %in% list("台南市","臺南市",
                     "台南縣","臺南縣",
                     "高雄縣","高雄市",
                     "屏東縣"), Region := "South"]%>%
  .[County %in% list("花蓮縣",
                     "台東縣","臺東縣"), Region := "East"] %>% 
  .[!is.na(Region),] %>% 
  .[, .(area=sum(Area)), by = list(Region)] %>% 
  .[, prob_Area:= (area/sum(area))] 

weight <- 
  M.data %>% 
  setDT %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>% 
  .[Year < 2019,] %>%
  .[!(TypeName.1 %in% "Not forest"), ] %>% 
  .[, SP := paste0(Site_N,"-",Point)]  %>%
  .[, list(Year, SP, Survey, Region)] %>%
  .[, point_n := .N, by = list(Year, SP, Region)] %>% 
  .[, SP_n:= .N, by = list(Region)] %>%
  left_join(county.area)  %>% setDT %>%
  .[, weight := (prob_Area / SP_n /point_n)] 




  .[!duplicated(.)] %>%
  .[, site_n :=length(unique(Site_N)), by = c( "cov1")]  %>%
  .[, point_n :=length(Point), by = c("Year", "Site_N", "cov1")]   %>%  #weight := prob_Area / site_n /調查次數
  .[, list(Year, Site_N, cov1, weight)]%>%
  .[!duplicated(.)] 




df <- 
  M.data %>% 
  setDT %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>% 
  .[Year < 2019,] %>%
  .[!(TypeName.1 %in% "Not forest"), ] %>% 
  .[, SP := paste0(Site_N,"-",Point)] %>% 
  .[, .(number = sum(Macaca_sur)), by = list(Year, SP, Region)] %>% 
  .[, N := sum(number), by = list(SP, Region)] %>% 
  .[!(N %in% 0),] %>% 
  .[, N := NULL] %>% 
  .[, Region := factor(Region)] %>% 
  setDF

m1 <- trim(df,
           count_col = "number",
           site_col = "SP",
           year_col = "Year",
           #weights_col = "weight",
           #covar_cols = "Region",
           model =  2,
           changepoints = "all",
           overdisp = T,
           serialcor = F, 
           autodelete = T, 
           stepwise = F)


summary(m1)
wald(m1)
totals(m1, "imputed", obs =F) %>% plot
index(m1, "imputed", covars = F) %>% plot
overall(m1,"imputed") %T>% plot



plot(overall(m1, "imputed"))
title( "imputed" ) 

heatmap(m1, "imputed") 
title( "imputed" )


index(m1, "imputed", covars = F) %>% plot(., pct = T, main = "imputed") 
index(m1, "imputed", covars = T) %>% plot(., pct = T, main = "imputed")




#East------
m2 <- trim(df[df$Region=="East",],
           count_col = "number",
           site_col = "SP",
           year_col = "Year",
           #weights_col = "weight",
           #covar_cols = "Region",
           model =  2,
           changepoints = "all",
           overdisp = F,
           serialcor = F, 
           autodelete = T, 
           stepwise = F)


overall(m2,"imputed") %T>% plot
index(m2, "imputed", covars = F) %>% plot(., pct = T, main = "imputed") 



#North------
m3 <- trim(df[df$Region=="North",],
           count_col = "number",
           site_col = "SP",
           year_col = "Year",
           #weights_col = "weight",
           #covar_cols = "Region",
           model =  2,
           changepoints = "all",
           overdisp = T,
           serialcor = F, 
           autodelete = T, 
           stepwise = F)


overall(m3,"imputed") %T>% plot
index(m3, "imputed", covars = F) %>% plot(., pct = T, main = "imputed") 


#Center------
m4 <- trim(df[df$Region=="Center",],
           count_col = "number",
           site_col = "SP",
           year_col = "Year",
           #weights_col = "weight",
           #covar_cols = "Region",
           model =  2,
           changepoints = "all",
           overdisp = F,
           serialcor = F, 
           autodelete = T, 
           stepwise = F)


overall(m4,"imputed") %T>% plot
index(m4, "imputed", covars = F) %>% plot(., pct = T, main = "imputed") 


#South------
m5 <- trim(df[df$Region=="South",],
           count_col = "number",
           site_col = "SP",
           year_col = "Year",
           #weights_col = "weight",
           #covar_cols = "Region",
           model =  2,
           changepoints = "all",
           overdisp = F,
           serialcor = F, 
           autodelete = T, 
           stepwise = F)


overall(m5,"imputed") %T>% plot
index(m5, "imputed", covars = F) %>% plot(., pct = T, main = "imputed") 











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
