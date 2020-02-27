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


d2<- dredge(m2, trace = T)

importance(d2)


#anova table==============================================
Anova(m2)



summary(glht(m2, linfct = mcp(Region2 = "Tukey")))

summary(glht(m2, linfct = c("Year.re = 0",
                            "Altitude.1 = 0",
                            "julian.D.1 = 0"))) 
par(mai=c(1,1.5,1,1))
glht(m2, linfct = mcp(Region2 = "Tukey")) %>% plot

par(mai=c(1,1,1.2,1))
plot(cld(glht(m2, linfct = mcp(Region2 = "Tukey"))), axes=F)
axis(2)
axis(1,at=1:6,labels = c("中彰投","雲嘉南","花蓮", "台東",  "北部","高屏"))
box()




summary(m2)


Anova(m2)

new.data <-  expand.grid(
  Region2=unique(df$Region2),
  Year=2015:2019,
  julian.D=seq(60,180,5),
  Altitude=seq(0,3800,100)) %>% setDT %>% 
  .[, Year.re := Year - min(Year) + 1] %>% 
  .[, Altitude.1 :=  scale(Altitude,scale =T)] %>% 
  .[, julian.D.1 :=  scale(julian.D,scale =T)] 

pp <- 
  cbind(new.data,
        PREDICT = predict(m2,new.data,re.form= ~0, type = c( "response"))
  )


plot(pp$Region2, pp$PREDICT)
plot(pp$Year, pp$PREDICT)
plot(pp$julian.D, pp$PREDICT)
plot(pp$Altitude, pp$PREDICT)
points(pp[pp$Region2 =="East2",]$Altitude, pp[pp$Region2 =="East2",]$PREDICT,col=2)
points(pp[pp$Region2 =="East1",]$Altitude, pp[pp$Region2 =="East1",]$PREDICT,col=3)
points(pp[pp$Region2 =="Center2",]$Altitude, pp[pp$Region2 =="Center2",]$PREDICT,col=6)
points(pp[pp$Region2 =="Center1",]$Altitude, pp[pp$Region2 =="Center1",]$PREDICT,col=5)
points(pp[pp$Region2 =="North",]$Altitude, pp[pp$Region2 =="North",]$PREDICT,col=4)




pp2 <- 
cbind(unique(df),
      PREDICT = predict(m2,unique(df),re.form= ~(1|Site_N), type = c( "response"))
) 

plot(pp2$Region2, pp2$PREDICT)
plot(pp2$Year, pp2$PREDICT)
plot(pp2$julian.D, pp2$PREDICT)
plot(pp2$Altitude, pp2$PREDICT)




#-----
m3 <- glmer(Macaca_sur ~  Altitude.1 + Year.re + julian.D.1 +  Region2 + (1|Site_N), 
            family = binomial, data = df[df$julian.D>75,],
            control = glmerControl(optimizer = "bobyqa"))
Anova(m3)



#-------------------------------------------

m4 <- glmer(Macaca_sur ~ I(Altitude.1^2) + Altitude.1 + Year.re + julian.D.1 +  Region2 + (1|Site_N), 
            family = binomial, data = df,
            control = glmerControl(optimizer = "bobyqa"))



summary(m4)
Anova(m4)


pp3 <- 
  cbind(new.data,
        PREDICT = predict(m4,new.data,re.form= ~0, type = c( "response"))
  )


plot(pp3$Region2, pp3$PREDICT)
plot(pp3$Year, pp3$PREDICT)
plot(pp3$julian.D, pp3$PREDICT)
plot(pp3$Altitude, pp3$PREDICT)

