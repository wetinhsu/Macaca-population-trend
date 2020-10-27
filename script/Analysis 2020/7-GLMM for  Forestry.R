# data analysis

library(tidyverse)
library(lme4)
library(car)
library(multcomp)
library(readxl)
library(MuMIn)
#------------------------------------------------
#Original data---- 

M.data <- read_excel("./data/clean/for analysis Forestrydata_V1.xlsx",
                     sheet="Data")  %>% 
  mutate(Office = ordered(Office,
                          levels = c("羅東", "新竹", "東勢", "南投",
                                     "嘉義", "屏東", "花蓮", "臺東"),
                          labels = c("Luodong", "Hsinchu", "Dougshih", "Nantou",
                                     "Chiayi", "Pingtung", "Hualien", "Taitung"))) %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(Survey = as.numeric(Survey)) %>% 
  mutate(Point = as.numeric(Point)) %>% 
  mutate(Month = as.numeric(Month)) %>% 
  mutate(Day = as.numeric(Day)) %>% 
  mutate(Macaca_sur = as.numeric(Macaca_sur)) %>% 
  mutate(Distance = as.numeric(Distance)) %>% 
  
  mutate(TypeName.1 = gsub("闊葉林", "broad_leaved", TypeName.1)) %>% 
  mutate(TypeName.1 = gsub("針葉林", "coniferous", TypeName.1)) %>% 
  mutate(TypeName.1 = gsub("竹林", "Bamboo", TypeName.1)) %>% 
  mutate(TypeName.1 = gsub("混淆林", "mixed", TypeName.1)) %>% 
  mutate(TypeName.1 = gsub("非森林", "Not forest", TypeName.1)) %>% 
  
  filter(analysis %in% "Y")


#---------------------------------------------------------------------

M.data <- M.data %>% 
  mutate( Year.re = Year - min(Year) + 1)


#==============================================
df <- 
  M.data 


#-------------------------------------------

allFit(glmer(Macaca_sur ~ TypeName.1  +  Altitude + julian.D + Office +  (1|Site_N), 
             family = binomial, data = df))   #嘗試使用一系列優化程序重新擬合glmer模型



df$Altitude.1 <-  scale(df$Altitude,scale =T)
df$julian.D.1 <-  scale(df$julian.D,scale =T)

m1 <- glmer(Macaca_sur ~  TypeName.1 + Altitude.1 + julian.D.1 + Office  + (1|Site_N), 
            family = binomial, data = df,
            control = glmerControl(optimizer = "bobyqa"))


Anova(m1)
summary(m1)



#AICc==============================================
options(na.action = "na.fail")
d1<- dredge(m1,  trace = T)

d1

summary(model.avg(d1))
summary(model.avg(d1, subset = delta < 2))

importance(d1)

sw(model.avg(d1, subset = delta < 2))



#-------------------------------------------


summary(glht(m1, linfct = mcp(Office = "Tukey")))

summary(glht(m1, linfct = c("Altitude.1 = 0",
                            "julian.D.1 = 0"))) 

par(mai=c(1,1,1,0.2))
glht(m1, linfct = mcp(Office = "Tukey")) %>% cld()  %>% plot

par(mai=c(1,1.5,1,1))
glht(m1, linfct = mcp(Office = "Tukey")) %>% plot





