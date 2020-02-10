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



summary(m1)


#anova table==============================================
Anova(m1)


summary(glht(m1, linfct = mcp(TypeName.1 = "Tukey")))
summary(glht(m1, linfct = mcp(Region2 = "Tukey")))

summary(glht(m1, linfct = c("Year.re = 0",
                            "Altitude.1 = 0",
                            "julian.D.1 = 0"))) 

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

allFit(glmer(Macaca_sur ~ Altitude + Year.re + julian.D +  Region2 + (1|Site_N), 
             family = binomial, data = df))   #嘗試使用一系列優化程序重新擬合glmer模型

allFit(glmer(Macaca_sur ~  Altitude.1 + Year.re + julian.D.1 +  Region2 + (1|Site_N), 
             family = binomial, data = df))   #嘗試使用一系列優化程序重新擬合glmer模型

m2 <- glmer(Macaca_sur ~ Altitude.1 + Year.re  +  julian.D.1 +  Region2 + (1|Site_N), 
            family = binomial, data = df,
            control = glmerControl(optimizer = "bobyqa"))

summary(m2)

Anova(m2)


