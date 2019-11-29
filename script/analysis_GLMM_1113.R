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

M.data <- 
  read_xlsx("data/clean/data_for_analysis_1519.xlsx") %>% 
  setDT %>% 
  .[ Distance <20 , ] %>%
  .[, Year := as.numeric(Year)] %>% 
  .[, Year.re := Year - min(Year) + 1] %>%
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
  .[  Region %in% "East", Region.2 := "East"] %>%
  .[!(Region %in% "East"), Region.2 := "Wast"] %>%
  .[  Region %in% "North", Region.3 := "North"] %>%
  .[ Region %in% "South", Region.3 := "South"]  %>%
  .[ Region %in% "Center", Region.3 := "South"] %>%
  .[ Region %in% "East", Region.3 := "South"] %>%
  .[ , County := factor(County, labels = 1:23)] %>%
  .[Year <2019,] %>%
  .[is.na(Macaca_sur), Macaca_sur := 0]


M.data$TypeName.1 %<>% as.factor    # factor(., labels = 1:4) # 竹林 = 1  針葉林= 2 混淆林=3 闊葉林=4
M.data$TypeName.1 %<>% as.factor
M.data$Altitude %<>% as.factor
M.data$Year %<>% as.numeric
M.data$Survey %<>% as.factor
M.data$Region %<>% as.factor
M.data$Region.2 %<>% as.factor
M.data$Region.3 %<>% as.factor


M.data %>% .[, list(Macaca_sur, TypeName.1, Survey, Year, County, Region, Altitude)]  %>% plot
#========================================================
str(M.data)

M.data %>%na.exclude %>%
  .[, list(TypeName.1 = as.numeric(TypeName.1),
           Macaca_sur = as.numeric(Macaca_sur),
           Survey = as.numeric(Survey),
           Year = as.numeric(Year),
           County = as.numeric(County),
           #Region.2 = as.numeric(Region.2), 
           Altitude = as.numeric(Altitude))]  %>% setDF %>%
  corPlot(.,stars=T,numbers=T, upper=F)




m1 <- glmer(Macaca_sur ~ TypeName.1  + Year.re +  Survey + (1|Site_N), 
            family = binomial, data = df)
Anova(m1)
print(summary(m1),correlation=FALSE)
anova(m1)

 +  Region.2


summary(glht(m1, linfct = mcp(TypeName.1 = "Tukey")))
summary(glht(m1, linfct = mcp(Year.re = "Tukey")))
summary(glht(m1, linfct = mcp(Survey = "Tukey")))
summary(glht(m1, linfct = mcp(Altitude = "Tukey")))
summary(glht(m1, linfct = mcp(Region.2 = "Tukey")))







#=====================

### Summary

M.data %>% setDT %>% .[is.na(Macaca_sur),Macaca_sur:=0 ] %>%
  .[!is.na(TypeName.1), .(M = sum(Macaca_sur, na.rm=T)), by = list(Year, Survey, TypeName.1)] %>% 
  dcast(Year + Survey ~ TypeName.1, value.var = "M")

M.data %>% setDT %>% .[is.na(Macaca_sur),Macaca_sur:=0 ] %>%
  .[!is.na(TypeName.1), .(Mean = round(mean(Macaca_sur, na.rm=T), 3),
                          SD = round(sd(Macaca_sur, na.rm=T), 3)),
    by = list(Year, TypeName.1)] %>% 
  dcast(Year ~ TypeName.1, value.var = c("Mean", "SD"))

M.data %>% setDT %>% .[is.na(Macaca_sur),Macaca_sur:=0 ] %>%
  .[!is.na(TypeName.1), .(Mean = mean(Macaca_sur, na.rm=T),
                          SD = sd(Macaca_sur, na.rm=T)/sqrt(length(Macaca_sur))), 
    by = Year]


bb<- M.data %>% setDT %>% .[is.na(Macaca_sur),Macaca_sur:=0 ] %>%
  .[!is.na(TypeName.1), .(Mean = mean(Macaca_sur, na.rm=T),
                          SD = sd(Macaca_sur, na.rm=T)/sqrt(length(Macaca_sur)),
                          n = .N), 
    by = list(TypeName.1,Survey, Altitude, Region.2)] %>%
  .[, N:= sum(n)]


sum(bb$N*(bb$N-bb$n)*(bb$SD)^2/bb$n)/(unique(bb$N)^2)
mean(bb$Mean)



#===================================================================

#
M.data %<>% .[!is.na( TypeName.1),]
m0 <-  glmer(Macaca_sur ~ (1|Site_N), 
            family = binomial, data = M.data)

print(summary(m0),correlation=FALSE)

m1 <- update(m0, ~ .+ TypeName.1)
m2 <- update(m0, ~ .+ Year.re)
m3 <- update(m0, ~ .+ Altitude)
m4 <- update(m0, ~ .+ Survey)
m5 <- update(m0, ~ .+ Region.2 )

m1.2 <- update(m1, ~ . + Year.re)
m1.3 <- update(m1, ~ . + Altitude)
m1.4 <- update(m1, ~ . + Survey)
m1.5 <- update(m1, ~ . + Region.2)
m2.3 <- update(m2, ~ . + Altitude)
m2.4 <- update(m2, ~ . + Survey)
m2.5 <- update(m2, ~ . + Region.2)
m3.4 <- update(m3, ~ . + Survey)
m3.5 <- update(m3, ~ . + Region.2)
m4.5 <- update(m4, ~ . + Region.2)

m1.2.3 <- update(m1.2, ~ . + Altitude)
m1.2.4 <- update(m1.2, ~ . + Survey)
m1.2.5 <- update(m1.2, ~ . + Region.2)
m1.3.4 <- update(m1.3, ~ . + Survey)  #failed to converge
m1.3.5 <- update(m1.3, ~ . + Region.2)
m1.4.5 <- update(m1.4, ~ . + Region.2)
m2.3.4 <- update(m2.3, ~ . + Survey)
m2.3.5 <- update(m2.3, ~ . + Region.2)
m2.4.5 <- update(m2.4, ~ . + Region.2)
m3.4.5 <- update(m3.4, ~ . + Region.2)

m1.2.3.4 <- update(m1.2.3, ~ . + Survey) #failed to converge
m1.2.3.5 <- update(m1.2.3, ~ . + Region.2) #failed to converge
m1.2.4.5 <- update(m1.2.4, ~ . + Region.2)
m1.3.4.5 <- update(m1.3.4, ~ . + Region.2) #failed to converge
m2.3.4.5 <- update(m2.3.4, ~ . + Region.2)

m1.2.3.4.5 <- update(m1.2.3.4, ~ . + Region.2) #failed to converge

AICtab(m0, m1, m2, m3, m4, m5,
       m1.2, m1.3, m1.4, m1.5, m2.3, m2.4, m2.5, m3.4, m3.5, m4.5,
       m1.2.3, m1.2.4, m1.2.5, m1.3.5, m1.4.5, m2.3.4, m2.3.5, m2.4.5, m3.4.5,
       m1.2.4.5, m2.3.4.5, 
       weights=TRUE, base = T,logLik=TRUE) 


AICtab(m0, m1, m2, m3, m4, m5,
       m1.2, m1.3, m1.4, m1.5, m2.3, m2.4, m2.5, m3.4, m3.5, m4.5,
       m1.2.3, m1.2.4, m1.2.5, m1.3.5, m1.4.5, m2.3.4, m2.3.5, m2.4.5, m3.4.5,
       m1.2.4.5, m2.3.4.5, 
       weights=TRUE, base = T,logLik=TRUE) %>%
print(.,min.weight=0.01)





#===================================================================

#
M.data %<>% .[!is.na( TypeName.1),]
m0 <-  glmer(Macaca_sur ~ (1|Site_N), 
             family = binomial, data = M.data)

print(summary(m0),correlation=FALSE)

m1 <- update(m0, ~ .+ TypeName.1)
m2 <- update(m0, ~ .+ Year.re)
m3 <- update(m0, ~ .+ Altitude)
m4 <- update(m0, ~ .+ Survey)
m5 <- update(m0, ~ .+ Region )

m1.2 <- update(m1, ~ . + Year.re)
m1.3 <- update(m1, ~ . + Altitude)
m1.4 <- update(m1, ~ . + Survey)
m1.5 <- update(m1, ~ . + Region)
m2.3 <- update(m2, ~ . + Altitude)
m2.4 <- update(m2, ~ . + Survey)
m2.5 <- update(m2, ~ . + Region)
m3.4 <- update(m3, ~ . + Survey)
m3.5 <- update(m3, ~ . + Region)
m4.5 <- update(m4, ~ . + Region)

m1.2.3 <- update(m1.2, ~ . + Altitude)
m1.2.4 <- update(m1.2, ~ . + Survey)
m1.2.5 <- update(m1.2, ~ . + Region)  #failed to converge
m1.3.4 <- update(m1.3, ~ . + Survey)  #failed to converge
m1.3.5 <- update(m1.3, ~ . + Region)  #failed to converge
m1.4.5 <- update(m1.4, ~ . + Region)  #failed to converge
m2.3.4 <- update(m2.3, ~ . + Survey)
m2.3.5 <- update(m2.3, ~ . + Region)
m2.4.5 <- update(m2.4, ~ . + Region)
m3.4.5 <- update(m3.4, ~ . + Region)

m1.2.3.4 <- update(m1.2.3, ~ . + Survey) #failed to converge
m1.2.3.5 <- update(m1.2.3, ~ . + Region) #failed to converge
m1.2.4.5 <- update(m1.2.4, ~ . + Region) #failed to converge
m1.3.4.5 <- update(m1.3.4, ~ . + Region) #failed to converge
m2.3.4.5 <- update(m2.3.4, ~ . + Region) #failed to converge

m1.2.3.4.5 <- update(m1.2.3.4, ~ . + Region) #failed to converge

AICtab(m0, m1, m2, m3, m4, m5,
       m1.2, m1.3, m1.4, m1.5, m2.3, m2.4, m2.5, m3.4, m3.5, m4.5,
       m1.2.3, m1.2.4, m2.3.4, m2.3.5, m2.4.5, m3.4.5,
       weights=TRUE, base = T,logLik=TRUE) 


anova(m0, m1, m2, m3, m4, m5,
       m1.2, m1.3, m1.4, m1.5, m2.3, m2.4, m2.5, m3.4, m3.5, m4.5,
       m1.2.3, m1.2.4, m2.3.4, m2.3.5, m2.4.5, m3.4.5)


(msAICc <- model.sel(m0, m1, m2, m3, m4, m5,
                     m1.2, m1.3, m1.4, m1.5, m2.3, m2.4, m2.5, m3.4, m3.5, m4.5,
                     m1.2.3, m1.2.4, m2.3.4, m2.3.5, m2.4.5, m3.4.5))
summary(model.avg(msAICc, subset = delta < 2))

get.models(msAICc, subset = delta < 2)
$coefficients

importance(msAICc)
#==============================================
df <- 
  M.data %>% 
  .[!is.na(TypeName.1), ] 


m1 <- glmer(Macaca_sur ~ TypeName.1  + Year.re + Altitude + Survey+  Region + (1|Site_N), 
            family = binomial, data = df)

options(na.action = "na.fail")
d1<- dredge(
  glmer(Macaca_sur ~ Region + Survey+ Altitude + Year.re + TypeName.1  +   (1|Site_N), 
        family = binomial, data = df), 
  subset = ((TypeName.1  && Year.re) | !Region) &&
    ((TypeName.1  && Altitude) | !Survey) ,
  trace = T, 
  extra = list(
    "R^2", "*" = function(x) {
      s <- summary(x)
      c(Rsq = s$r.squared, adjRsq = s$adj.r.squared,
        F = s$fstatistic[[1]])}))

summary(model.avg(d1, subset = delta < 2))

