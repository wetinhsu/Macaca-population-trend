# data analysis

library(data.table)
library(magrittr)
library(multcomp)
library(ggplot2)
library(rtrim)
library(dplyr)
library(openxlsx)
library(rtrim)
library(readxl)
#--------------------------------

#Original data---- 

M.data <- read_excel("./data/clean/for analysis_V1.xlsx",
                     sheet=1) %>% setDT %>% 
  #.[, DATE := as.IDate(paste(Year, Month, Day, sep = "/"))] %>% 
  .[TypeName %like% "混", TypeName.n := "mixed"] %>% 
  .[TypeName %like% "竹林", TypeName.n := "Bamboo"] %>% 
  .[TypeName %like% "闊葉樹林型", TypeName.n := "broad-leaved"] %>% 
  .[TypeName %like% "針葉樹林型", TypeName.n := "coniferous"] %>% 
  .[, TypeName.1 := ifelse(Distance>20, "Not forest", TypeName.n)] 
  

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
M.data$Region2 %<>% as.factor


county.area <- read.csv("./data/clean/gis/county area-2.csv", header = T) %>% 
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
  
  
  
  .[County %in% list("宜蘭縣","基隆市","台北市","臺北市",
                     "新北市","台北縣","臺北縣",
                     "桃園縣","桃園市","新竹市",
                     "新竹縣","苗栗縣"), Region2 := "North"] %>% 
  .[County %in% list("台中市","臺中市",
                     "台中縣","臺中縣",
                     "彰化縣","南投縣","南投市"), Region2 := "Center1"] %>% 
  .[County %in% list("雲林縣","嘉義縣","嘉義市",
                     "台南市","臺南市",
                     "台南縣","臺南縣"), Region2 := "Center2"] %>%
  .[County %in% list("高雄縣","高雄市",
                     "屏東縣"), Region2 := "South"]%>%
  .[County %in% list("花蓮縣"), Region2 := "East1"] %>%
  .[County %in% list("台東縣","臺東縣"), Region2 := "East2"] %>% 
  
  .[!is.na(Region2),] %>% 
  .[, .(area=sum(Area)), by = list(Region2)] %>% 
  .[, prob_Area:= (area/sum(area))] %>% 
  .[, Region2 := as.character(Region2)]

weight <- 
  M.data %>% 
  setDT  %>% 
  .[, Region2 := as.character(Region2)]%>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>% 
  #.[Year < 2019,] %>%
  .[!(TypeName.1 %in% "Not forest"), ] %>% 
  .[, SP := paste0(Site_N,"-",Point)]  %>%
  .[, list(Year, SP, Survey, Region2)] %>%
  .[, .(point_n = .N), by = list(Year, SP, Region2)] %>% 
  .[, SP_n := .N, by = list(Year,Region2)] %>%
  left_join(county.area)  %>% setDT %>%
  .[, weight := (prob_Area / SP_n /point_n)]  
  






df <- 
  M.data %>% 
  setDT %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>% 
  #.[Year < 2019,] %>%
  .[!(TypeName.1 %in% "Not forest"), ] %>% 
  .[, SP := paste0(Site_N,"-",Point)] %>% 
  .[, .(number = sum(Macaca_sur)), by = list(Year, SP, Region2, Altitude)] %>% 
  .[, N := sum(number), by = list(SP, Region2)] %>% 
  left_join(weight[, c(1:2, 8)])  %>% setDT %>%
  .[!(N %in% 0),] %>% 
  .[, N := NULL] %>% 
  .[, Altitude_f := cut(Altitude,
                        breaks = c(0,1000,2500,4000),
                        labels = c("A","B","C"),
                        include.lowest = T)] %>%
  .[, Altitude_f := factor(Altitude_f)] %>%
  .[, Region2 := factor(Region2)] %>% 
  .[,Altitude := NULL] %>% 
  setDF

#--

m1 <- trim(
           number ~ SP + Year + Region2 + Altitude_f,
           weights = "weight",df,
           model =  2,
           changepoints = "all",
           overdisp = F,
           serialcor = F, 
           autodelete = T, 
           stepwise = F)


summary(m1)
wald(m1)
totals(m1, "imputed", obs =F) %>% plot
index(m1, "imputed", covars = F) %>% plot
index(m1, "imputed", covars = T) %>% plot

overall(m1,"imputed") %T>% plot

index(m1, covars = F) %>% plot(., pct=T)
index(m1, covars = T) %>% plot(., covar = "Altitude_f")

plot(overall(m1, "imputed"), axes = F)
axis(1, at = 2015:2019, cex.axis=1.5)
axis(2, cex.axis=1.5)
box()
title( "imputed" ) 

heatmap(m1, "imputed", cex.axis=1.5) 
title( "imputed" )


index(m1, "imputed", covars = F) %>% plot(., pct = T, main = "imputed")

index(m1, "imputed", covars = T) %>% 
  plot(., pct = T, main = "imputed",
       axes = F,
       xaxs="i", cex.lab = 1.3)
axis(1, at = 2015:2019, cex.axis=1.5)
axis(2, cex.axis=1)
box()



idx <- index(m1, "imputed", covars = T)  %>% setDT %>%
  .[,list(covariate, category, time, imputed = imputed*100, se_imp = se_imp*100)] 



  ggplot(idx[idx$covariate %in% "Overall",], aes(x=time, y=imputed)) + 
  geom_errorbar(aes(ymin=imputed-se_imp, ymax=imputed+se_imp), width=.1, colour = '#f3e4c2', size = .9) +
  geom_line(colour = '#f3e4c2', size = 2, linetype = 1) +
  geom_point(colour='#f3e4c2', size = 8, shape = 21, stroke = 2, fill='#ef4926') +
  expand_limits(y = 0) +
  scale_x_continuous(breaks = 2015:2019)+
  #scale_y_continuous(breaks = c(100, 5000,10000))+
  # coord_cartesian(ylim=c(0,150)) +
  #theme with white background
  
  
  #eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1),
    text = element_text(size=30),
    axis.line.x = element_line(color="black", size = .1),
    axis.line.y = element_line(color="black", size = .1)) +
  geom_hline(yintercept=100, colour = "grey", linetype = 3)


  df %>% 
    reshape2::dcast(SP ~ Year, value.var = c("number")) %>% 
    View()

