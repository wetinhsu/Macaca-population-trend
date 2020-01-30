# data analysis

library(data.table)
library(lme4)
library(car)
library(magrittr)
library(multcomp)
library(ggplot2)
library(psych)
library(readxl)
library(MuMIn)
#------------------------------------------------

#Original data---- 

M.data.o <- read_excel("./data/clean/for analysis.xlsx",
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

M.data.o$Year %<>% as.numeric
M.data.o$Survey %<>% as.numeric
M.data.o$Point %<>% as.numeric
M.data.o$Macaca_sur %<>% as.numeric
M.data.o$Month %<>% as.numeric
M.data.o$Day %<>% as.numeric
M.data.o$Distance %<>% as.numeric
M.data.o$julian.D %<>% as.numeric
M.data.o$Region %<>% as.factor
M.data.o$TypeName.1 %<>% as.factor
M.data.o$Site_N %<>% as.factor

#Remove duplicate data-------------------------------------------
M.data <- M.data.o %>% copy(.) %>% 
  .[Year %in% 2015 & Survey %in% 2 & Site_N %in% "A29-17" & Point %in% 7,
    Macaca_sur := NA] %>% 
  .[Year %in% 2015 & Survey %in% 2 & Site_N %in% "A33-28" & Point %in% 7,
    Macaca_sur := NA] %>% 
  .[Year %in% 2016 & Survey %in% 1 & Site_N %in% "B33-01" & Point %in% 4,
    Macaca_sur := NA] %>% 
  .[Year %in% 2017 & Survey %in% 1 & Site_N %in% "B38-07" & Point %in% 7,
    Macaca_sur := NA] %>% 
  .[Year %in% 2018 & Survey %in% 1 & Site_N %in% "A35-15" & Point %in% 5,
    Macaca_sur := NA] %>% 
  .[Year %in% 2018 & Survey %in% 2 & Site_N %in% "A28-10" & Point %in% 6,
    Macaca_sur := NA] %>% 
  .[Year %in% 2019 & Survey %in% 1 & Site_N %in% "B14-02" & Point %in% 6,
    Macaca_sur := NA] %>% 
  .[Year %in% 2019 & Survey %in% 1 & Site_N %in% "B38-08" & Point %in% 5,
    Macaca_sur := NA] %>% 
  .[Year %in% 2019 & Survey %in% 2 & Site_N %in% "A20-02" & Point %in% 3,
    Macaca_sur := NA] %>% 
  .[Year %in% 2019 & Survey %in% 2 & Site_N %in% "A33-32" & Point %in% 6,
    Macaca_sur := NA] 
#---------------------------------------------------------------------

M.data <- M.data %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>% 
  .[, Year := as.numeric(Year)] %>% 
  .[, Year.re := Year - min(Year) + 1]

M.data %>%
  .[Macaca_sur %in% 1, .N, list(TypeName.1, Macaca_dist)] %>%
  dcast(.,Macaca_dist ~TypeName.1, value.var="N")

#==============================================
df <- 
  M.data %>% 
  .[Year < 2019,] %>%
  .[!(TypeName.1 %in% "Not forest"), ]

#-------------------------------------------

allFit(glmer(Macaca_sur ~ TypeName.1  + Year.re + Altitude + julian.D +  Region + (1|Site_N), 
             family = binomial, data = df))   #嘗試使用一系列優化程序重新擬合glmer模型



df$Altitude.1 <-  scale(df$Altitude,scale =T)
df$julian.D.1 <-  scale(df$julian.D,scale =T)

m1 <- glmer(Macaca_sur ~  Year.re + TypeName.1 + Altitude.1 + julian.D.1 +  Region + (1|Site_N), 
            family = binomial, data = df,
            control = glmerControl(optimizer = "bobyqa"))

summary(m1)

#anova table==============================================
Anova(m1)


summary(glht(m1, linfct = mcp(TypeName.1 = "Tukey")))
summary(glht(m1, linfct = mcp(Survey = "Tukey")))
summary(glht(m1, linfct = mcp(Altitude.1 = "Tukey")))
summary(glht(m1, linfct = mcp(Region = "Tukey")))


#AICc==============================================
options(na.action = "na.fail")
d1<- dredge(
  glmer(Macaca_sur ~ TypeName.1  + Year.re + Altitude.1 + julian.D.1 +  Region + (1|Site_N), 
        family = binomial, data = df,
        control = glmerControl(optimizer = "bobyqa")), 
  trace = T)

summary(model.avg(d1))
summary(model.avg(d1, subset = delta < 2))

importance(d1)

sw(model.avg(d1, subset = delta < 2))



#Estimate2==============================================
##<25
aa<- df %>% setDT %>% 
  .[, A := ifelse(Macaca_dist %in% "A", Macaca_sur,0)] %>% 
  .[, AB := ifelse(Macaca_dist %in% c("A","B"), Macaca_sur,0)]  %>% 
  
  .[, .(Mean = mean(A, na.rm=T),
        SD = sd(A, na.rm=T)/sqrt(length(A)),
        n = .N), 
    by = list(TypeName.1,Survey, Region)] %>%
  .[, N:= sum(n)]


sum(aa$N*(aa$N-aa$n)*(aa$SD)^2/aa$n, na.rm=T)/(unique(aa$N)^2)
mean(aa$Mean)



##<100
bb<- df %>% setDT %>% 
  .[, A := ifelse(Macaca_dist %in% "A", Macaca_sur,0)] %>% 
  .[, AB := ifelse(Macaca_dist %in% c("A","B"), Macaca_sur,0)]  %>% 
  
  .[, .(Mean = mean(AB, na.rm=T),
        SD = sd(AB, na.rm=T)/sqrt(length(AB)),
        n = .N), 
    by = list(TypeName.1,Survey,  Region)] %>%
  .[, N:= sum(n)]


sum(bb$N*(bb$N-bb$n)*(bb$SD)^2/bb$n, na.rm=T)/(unique(bb$N)^2)
mean(bb$Mean)


#Estimate3==============================================
##<25
aa<- df %>% setDT %>% 
  .[, A := ifelse(Macaca_dist %in% "A", Macaca_sur,0)] %>% 
  .[, AB := ifelse(Macaca_dist %in% c("A","B"), Macaca_sur,0)]  %>% 
  .[, julian.D_f := cut(julian.D,
                        breaks = c(seq(0,210,15)),
                        include.lowest = T)] %>% 
  .[, .(Mean = mean(A, na.rm=T),
        SD = sd(A, na.rm=T)/sqrt(length(A)),
        n = .N), 
    by = list(TypeName.1,julian.D_f,  Region)] %>%
  .[, N:= sum(n)]


(se.25 <- sum(aa$N*(aa$N-aa$n)*(aa$SD)^2/aa$n, na.rm=T)/(unique(aa$N)^2))
mean(aa$Mean)

se.25^0.5*1.28  #80%CI=se*1.28


##<100
bb<- df %>% setDT %>% 
  .[, A := ifelse(Macaca_dist %in% "A", Macaca_sur,0)] %>% 
  .[, AB := ifelse(Macaca_dist %in% c("A","B"), Macaca_sur,0)]  %>% 
  .[, julian.D_F := cut(julian.D,
                        breaks = c(seq(0,210,15)),
                        include.lowest = T)] %>% 
  
  .[, .(Mean = mean(AB, na.rm=T),
        SD = sd(AB, na.rm=T)/sqrt(length(AB)),
        n = .N), 
    by = list(TypeName.1,julian.D_f,  Region)] %>%
  .[, N:= sum(n)]


(se.100 <- sum(bb$N*(bb$N-bb$n)*(bb$SD)^2/bb$n, na.rm=T)/(unique(bb$N)^2))
mean(bb$Mean)

se.100^0.5*1.28
