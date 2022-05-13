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

M.data <- read_excel("./data/clean/for analysis_1521.xlsx",
                     sheet=1) %>% setDT %>% 
  .[analysis %in% "Y",] %>% 
  
  .[TypeName.1 %like% "混淆林", TypeName.1 := "mixed"] %>% 
  .[TypeName.1 %like% "竹林", TypeName.1 := "Bamboo"] %>% 
  .[TypeName.1 %like% "闊葉林", TypeName.1 := "broad-leaved"] %>% 
  .[TypeName.1 %like% "針葉林", TypeName.1 := "coniferous"] %>% 
  .[TypeName.1 %like% "非森林", TypeName.1 := "Not forest"] %>% 

  
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

#---------------------------------------------------------------------

M.data <- M.data %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>% 
  .[, Year := as.numeric(Year)] %>% 
  .[, Year.re := Year - min(Year) + 1]



#==============================================
df <- 
  M.data 

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
d1<- dredge(m1, trace = T)

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


#-----
ND <- 
df %>% 
  dplyr::select(Year.re, Region2, julian.D.1, Year, julian.D, Site_N, Altitude.1, Altitude) 

ND$pred= predict(m2, ND,re.form=NA,
               type="response")

ND %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(Mean = mean(pred), 
                   DL = quantile(pred,0.025),
                   UL = quantile(pred,0.975)) %>% 
  ggplot(., aes(x= Year, y = Mean))+
  geom_ribbon(aes(ymin = DL, ymax = UL), fill = "grey80")+
  geom_point()+
  geom_line(lty = 1)+
  theme_bw()
  





