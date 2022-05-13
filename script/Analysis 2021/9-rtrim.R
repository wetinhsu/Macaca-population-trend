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

M.data <- read_excel("./data/clean/for analysis_V2.xlsx",
                     sheet=1) %>% setDT %>% 

  .[, Year := as.numeric(Year)] %>% 
  .[, Survey := as.numeric(Survey)] %>% 
  .[, Point := as.numeric(Point)] %>% 
  .[, Macaca_sur := as.numeric(Macaca_sur)] %>% 
  .[, Month := as.numeric(Month)] %>% 
  .[, Day := as.numeric(Day)] %>% 
  .[, Distance := as.numeric(Distance)] %>% 
  .[, julian.D := as.numeric(julian.D)] %>% 
  .[, TypeName.1 := as.factor(TypeName.1)] %>% 
  .[, Site_N := as.factor(Site_N)] %>% 
  .[, Region2 := as.factor(Region2)] 

#----------------------------

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
                     "彰化縣","南投縣","南投市"), Region2 := "Center"] %>% 
  .[County %in% list("雲林縣","嘉義縣","嘉義市",
                     "台南市","臺南市",
                     "台南縣","臺南縣"), Region2 := "Southwest"] %>%
  .[County %in% list("高雄縣","高雄市",
                     "屏東縣"), Region2 := "South"]%>%
  .[County %in% list("花蓮縣"), Region2 := "Hualien"] %>%
  .[County %in% list("台東縣","臺東縣"), Region2 := "Taitung"] %>% 
  
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
  number ~ SP + Year + Region2 ,
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
index(m1, "imputed", covars = F) %>% plot(., pct =T)
index(m1, "imputed", covars = T) %>% plot

overall(m1,"imputed") %T>% plot

index(m1, covars = F) %>% plot(., pct=F)


plot(overall(m1, "imputed"), axes = F)
axis(1, at = 2015:2019, cex.axis=1.5)
axis(2, cex.axis=1.5)
box()
title( "imputed" ) 

heatmap(m1, "imputed", cex.axis=1.5) 
title( "imputed" )



idx <- index(m1, "imputed", covars = T)  %>% setDT %>%
  .[,list(covariate, category, time, imputed = imputed, se_imp = se_imp)] 



ggplot(idx[idx$covariate %in% "Overall",], aes(x=time, y=imputed)) +
  #    geom_errorbar(aes(ymin=imputed-se_imp, ymax=imputed+se_imp), width=.1, color = gray(0.7), size = .09) +
  geom_line(colour = 'black', size = 1, linetype = 5) +
  geom_point(colour='black', size = 5, shape = 21, fill='red',stroke = 2) +
  geom_hline(yintercept=1, colour = "grey", linetype = 3)+
  expand_limits(y = 0) +
  scale_x_continuous(breaks = 2015:2019)+
  scale_y_continuous(breaks = c(-1000,1,1000,2000,3000))+
  coord_cartesian(ylim=c(-1500,3000)) +
  labs(x= "Year", y = "Index") + 
  #theme with white background
  theme_bw()+
  
  #eliminates background, gridlines, and chart border
  theme(
    plot.margin = margin(30,30,20,20),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #    panel.border = element_blank(),
    
    axis.title = element_blank(),
    #    axis.text.x = element_text(angle = 0, hjust = 1),
    text = element_text(size=20, color = "black"),
    axis.line.x = element_line(color="black", size = .1),
    axis.line.y = element_line(color="black", size = .1)
  ) 



df %>% 
  reshape2::dcast(SP ~ Year, value.var = c("number")) %>% 
  View()


#--------- 
df2 <- df %>% setDT %>% 
  .[Year %in% c(2015:2017,2019),] %>% 
  .[,Year2 := ifelse(Year %in% 2019, (Year-2014-1), (Year-2014))] %>% 
  setDF


m2 <- trim(
  number ~ SP + Year2 + Region2 ,
  weights = "weight",df2,
  model =  2,
  changepoints = "all",
  overdisp = F,
  serialcor = F, 
  autodelete = T, 
  stepwise = F)
index(m2, "imputed", covars = F) %>% plot(., pct =T, axes = F)
axis(1, at = 1:4, labels = c(2015:2017,2019))
axis(2)
box()


overall(m2,"imputed") %T>% plot

index(m1, "imputed", covars = F) %>% plot(., pct =T, axes = F)
axis(1)
axis(2)
box()
overall(m1,"imputed") %T>% plot


overall(m1)
overall(m2)



#---------

weight2 <- 
  M.data %>% 
  mutate(Region2 = Region2 %>% as.character) %>% 
  mutate(Macaca_sur = ifelse(is.na(Macaca_sur),0,  Macaca_sur))%>% 
  
  filter(!TypeName.1 %in% "Not forest") %>% 
  mutate(SP = paste0(Site_N,"-",Point))  %>%
  reshape2::dcast(SP + Region2  ~ Year) %>% 
  reshape2::melt(id = 1:2, variable.name = "Year", value.name = "point_n") %>% 
  mutate(point_n = ifelse(point_n %in% 0 ,1, point_n)) %>% 
  group_by(Region2) %>% 
  mutate(SP_n := SP %>% unique %>% length()) %>% 
  left_join(county.area)  %>% 
  mutate( weight= (prob_Area / SP_n /point_n)  ) %>% 
  select(SP, Year, Region2, weight) %>% 
  mutate(Year = Year %>% as.character() %>% as.integer())


df3 <- 
  M.data %>% 
  setDT %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>% 
  #.[Year < 2019,] %>%
  .[!(TypeName.1 %in% "Not forest"), ] %>% 
  .[, SP := paste0(Site_N,"-",Point)] %>% 
  .[, .(number = sum(Macaca_sur)), by = list(Year, SP, Region2, Altitude)] %>% 
  .[, N := sum(number), by = list(SP, Region2)] %>% 
  left_join(weight2)  %>% setDT %>%
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


m3 <- trim(
  number ~ SP + Year + Region2 ,
  weights = "weight",df3,
  model =  2,
  changepoints = "all",
  overdisp = F,
  serialcor = F, 
  autodelete = T, 
  stepwise = F)

index(m3, covars = F) %>% plot(., pct=F)
summary(m3)
wald(m3)
#-----------
