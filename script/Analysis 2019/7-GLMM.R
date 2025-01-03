# data analysis

library(data.table)
library(lme4)
library(car)
library(magrittr)
library(multcomp)
library(ggplot2)
library(readxl)
library(MuMIn)
#------------------------------------------------
#Original data---- 

M.data <- read_excel("./data/clean/for analysis_V1.xlsx",
                     sheet=1) %>% setDT %>% 
  .[analysis %in% "Y",] %>% 
  
  .[TypeName %like% "混", TypeName.n := "mixed"] %>% 
  .[TypeName %like% "竹林", TypeName.n := "Bamboo"] %>% 
  .[TypeName %like% "闊葉樹林型", TypeName.n := "broad-leaved"] %>% 
  .[TypeName %like% "針葉樹林型", TypeName.n := "coniferous"] %>% 
  .[, TypeName.1 := ifelse(Distance>20, "Not forest", TypeName.n)] %>% 
  
  .[, Year := as.numeric(Year)] %>% 
  .[, Survey := as.numeric(Survey)] %>% 
  .[, Point := as.numeric(Point)] %>% 
  .[, Macaca_sur := as.numeric(Macaca_sur)] %>% 
  .[, Month := as.numeric(Month)] %>% 
  .[, Day := as.numeric(Day)] %>% 
  .[, Distance := as.numeric(Distance)] %>% 
  .[, julian.D := as.numeric(julian.D)] %>% 
  .[, Region := as.factor(Region)] %>% 
  .[, TypeName.1 := as.factor(TypeName.1)] %>% 
  .[, Site_N := as.factor(Site_N)] %>% 
  .[, Region2 := as.factor(Region2)] 

#---------------------------------------------------------------------

M.data <- M.data %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>% 
  .[, Year := as.numeric(Year)] %>% 
  .[, Year.re := Year - min(Year) + 1]



#==============================================
df <- 
  M.data %>% 
  #.[Year < 2019,] %>%
  .[!(TypeName.1 %in% "Not forest"), ] %>% 
  .[Macaca_dist %in% "c", Macaca_sur :=0]

#-------------------------------------------

allFit(glmer(Macaca_sur ~ TypeName.1  + Year.re + Altitude + julian.D +  Region2 + (1|Site_N), 
             family = binomial, data = df))   #嘗試使用一系列優化程序重新擬合glmer模型



df$Altitude.1 <-  scale(df$Altitude,scale =T)
df$julian.D.1 <-  scale(df$julian.D,scale =T)

m1 <- glmer(Macaca_sur ~  TypeName.1 + Year.re + Altitude.1 + julian.D.1 +  Region2 + (1|Site_N), 
            family = binomial, data = df,
            control = glmerControl(optimizer = "bobyqa"))


Anova(m1)
summary(m1)


#AICc==============================================
options(na.action = "na.fail")
d1<- dredge(
  glmer(Macaca_sur ~ TypeName.1  + Year.re + Altitude.1 + julian.D.1 +  Region2 + (1|Site_N), 
        family = binomial, data = df,
        control = glmerControl(optimizer = "bobyqa")), 
  trace = T)

summary(model.avg(d1))
summary(model.avg(d1, subset = delta < 2))

importance(d1)

sw(model.avg(d1, subset = delta < 2))

#---------------------
m2 <- glmer(Macaca_sur ~ Altitude.1 + Year.re  +  julian.D.1 +  Region2 + (1|Site_N), 
            family = binomial, data = df,
            control = glmerControl(optimizer = "bobyqa"))



#anova table==============================================
Anova(m2)
summary(m2)


summary(glht(m2, linfct = mcp(Region2 = "Tukey")))

summary(glht(m2, linfct = c("Year.re = 0",
                            "Altitude.1 = 0",
                            "julian.D.1 = 0"))) 
par(mai=c(1,1.5,1,1))
glht(m2, linfct = mcp(Region2 = "Tukey")) %>% plot



