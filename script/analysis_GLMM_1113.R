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
  .[ Distance >20 | TypeName %in% c("待成林地", "裸露地", "陰影") , TypeName.1 := "Not forest"] %>%
  
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
  .[ Region %in% c("South", "Center", "East"), Region.3 := "South"]  %>%
  .[ , County := factor(County, labels = 1:25)] %>%
  .[is.na(Macaca_sur), Macaca_sur := 0]
  
  # 確認第1次調查，Survey為1 
M.data %<>%  dcast.data.table(.,Year + Site_N ~ Survey, value.var = "Macaca_sur") %>%  
                                                           #計算第1旅次及第2旅次調查的樣點數
  .[`1` %in% 0,] %>%  #找出有第2旅次沒第1旅次的樣區
  .[M.data, on = c("Year", "Site_N")] %>%  
  .[!is.na(`1`), Survey := 1] %>%  #將第1次調查的旅次改回1
  .[,`1` := NULL]%>%
  .[,`2` := NULL]



M.data$TypeName.1 %<>% factor(., labels = c("Not forest",1:4))  # 竹林 = 1  針葉林= 2 混淆林=3 闊葉林=4
M.data$TypeName.1 %<>% as.factor
M.data$Altitude %<>% as.factor
M.data$Year %<>% as.numeric
M.data$Survey %<>% as.factor
M.data$Region %<>% as.factor
M.data$Region.2 %<>% as.factor
M.data$Region.3 %<>% as.factor


M.data %>% .[, list(Macaca_sur, TypeName.1, Survey, Year, County, Region, Altitude)]  %>% plot
#========================================================

M.data %>% 
  
  dcast(.,Year + Survey ~ TypeName.1, value.var = "Macaca_sur", fun = c(length,sum))
  

M.data %>% 
  
  dcast(.,Year + Survey ~ TypeName.1 + Altitude, value.var = "Macaca_sur", fun = c(sum))



M.data %>% 
  
  dcast(., TypeName.1 ~ Altitude, value.var = "Macaca_sur", fun = c(sum))

M.data %>% 
  .[Macaca_sur %in% 1,] %>%
  dcast(.,Year  ~ Macaca_dist, value.var = "Macaca_sur", fun =sum)

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







#==============================================
df <- 
  M.data %>% 
  .[Year < 2019,] %>%
  .[!(TypeName.1 %in% "Not forest"), ] 

df$TypeName.1<- relevel(df$TypeName.1, ref = "4")
df$Region <- relevel(df$Region, ref = "East")




  
#full model==============================================
allFit(glmer(Macaca_sur ~ TypeName.1  + Year.re + Altitude + Survey +  Region + (1|Site_N), 
             family = binomial, data = df))   #嘗試使用一系列優化程序重新擬合glmer模型

m1 <- glmer(Macaca_sur ~ Year.re + TypeName.1 + Altitude + Survey+  Region + (1|Site_N), 
            family = binomial, data = df,
            control = glmerControl(optimizer = "bobyqa"))

summary(m1)

#anova table==============================================
Anova(m1)


summary(glht(m1, linfct = mcp(TypeName.1 = "Tukey")))
summary(glht(m1, linfct = mcp(Survey = "Tukey")))
summary(glht(m1, linfct = mcp(Altitude = "Tukey")))
summary(glht(m1, linfct = mcp(Region = "Tukey")))


#AICc==============================================
options(na.action = "na.fail")
d1<- dredge(
  glmer(Macaca_sur ~ TypeName.1  + Year.re + Altitude + Survey+  Region + (1|Site_N), 
        family = binomial, data = df,
        control = glmerControl(optimizer = "bobyqa")), 
  trace = T)

summary(model.avg(d1))
summary(model.avg(d1, subset = delta < 2))

importance(d1)

sw(model.avg(d1, subset = delta < 2))


#Estimate==============================================
bb<- df %>% setDT %>% 
  .[, A := ifelse(Macaca_dist %in% "A", Macaca_sur,0)] %>% 
  .[, AB := ifelse(Macaca_dist %in% c("A","B"), Macaca_sur,0)]  %>% 

  .[, .(Mean = mean(AB, na.rm=T),
                          SD = sd(AB, na.rm=T)/sqrt(length(AB)),
                          n = .N), 
    by = list(TypeName.1,Survey, Altitude, Region)] %>%
  .[, N:= sum(n)]


sum(bb$N*(bb$N-bb$n)*(bb$SD)^2/bb$n, na.rm=T)/(unique(bb$N)^2)
mean(bb$Mean)


aa<- df %>% setDT %>% 
  .[, A := ifelse(Macaca_dist %in% "A", Macaca_sur,0)] %>% 
  .[, AB := ifelse(Macaca_dist %in% c("A","B"), Macaca_sur,0)]  %>% 
  
  .[, .(Mean = mean(A, na.rm=T),
        SD = sd(A, na.rm=T)/sqrt(length(A)),
        n = .N), 
    by = list(TypeName.1,Survey, Altitude, Region)] %>%
  .[, N:= sum(n)]


sum(aa$N*(aa$N-aa$n)*(aa$SD)^2/aa$n, na.rm=T)/(unique(aa$N)^2)
mean(aa$Mean)

#Estimate2==============================================
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
