# data analysis

library(tidyverse)
library(lme4)
library(car)
library(multcomp)
library(readxl)
library(MuMIn)
library(emmeans)
#------------------------------------------------
#Original data---- 

M.data <- 
  list.files("./data/clean/Forestry/for analysis/", full.names = T,pattern = "xlsx$|xls$") %>% 
  lapply(., read_excel, sheet="Data", col_types = "text") %>% 
  bind_rows() %>% 
  
  mutate(Office = 
           ordered(Office,
                   levels = c("羅東", "新竹", "東勢", "南投",
                              "嘉義", "屏東", "花蓮", "臺東"),
                   labels = c("Luodong", "Hsinchu", "Dougshih", "Nantou",
                              "Chiayi", "Pingtung", "Hualien", "Taitung")
                   )
         ) %>% 
  
  mutate_at(c("Year", "Survey","Month",
              "Day", "Macaca_sur", "Distance", "julian.D", "Altitude"), as.numeric) %>% 
  
  mutate(TypeName.1 = case_when(
    TypeName.1 %in% "闊葉林" ~ "broad_leaved",
    TypeName.1 %in% "針葉林" ~ "coniferous",
    TypeName.1 %in% "竹林" ~ "Bamboo",
    TypeName.1 %in% "混淆林" ~ "mixed",
    TypeName.1 %in% "非森林" ~ "Not forest"
  )) %>% 
  
  filter(analysis %in% "Y")


#---------------------------------------------------------------------

M.data <- M.data %>% 
  mutate( Year.re = Year - min(Year) + 1)


#==============================================
df <- 
  M.data 


#-------------------------------------------

allFit(glmer(Macaca_sur ~ TypeName.1  + Year.re + Altitude + julian.D + Office +  (1|Site_N), 
             family = binomial, data = df))   #嘗試使用一系列優化程序重新擬合glmer模型



df$Altitude.1 <-  scale(df$Altitude,scale =T)
df$julian.D.1 <-  scale(df$julian.D,scale =T)

m1 <- glmer(Macaca_sur ~  TypeName.1 + Year.re + Altitude.1 + julian.D.1 + Office  + (1|Site_N), 
            family = binomial, data = df,
            control = glmerControl(optimizer = "bobyqa"))


Anova(m1)
summary(m1)


qqnorm(resid(m1))
qqline(resid(m1))
plot(fitted(m1),resid(m1))
#AICc==============================================
options(na.action = "na.fail")
d1<- dredge(m1,  trace = T)

d1

summary(model.avg(d1))
summary(model.avg(d1, subset = delta < 2))

importance(d1)

sw(model.avg(d1, subset = delta < 2))


m1.1 <- glmer(Macaca_sur ~   Year.re +  julian.D.1 + Office  + (1|Site_N), 
            family = binomial, data = df,
            control = glmerControl(optimizer = "bobyqa"))
Anova(m1.1)

#-------------------------------------------


#summary(glht(m1, linfct = mcp(Office = "Tukey")))  
#2021年有跳出warning，所以找新的指令，也是事後檢定

summary(as.glht(pairs(emmeans(m1.1, ~Office))), test=adjusted("free"))

summary(glht(m1, linfct = c("Year.re = 0",
                            "Altitude.1 = 0",
                            "julian.D.1 = 0"))) 

par(mai=c(1,1,1.5,0.2))
glht(m1, linfct = mcp(Office = "Tukey")) %>% cld()  %>% plot

par(mai=c(1,1.5,1,1))
glht(m1, linfct = mcp(Office = "Tukey")) %>% plot



M.data %>% 
  group_by(Year, Survey) %>% 
  summarise(
    N = n(),
    m = Macaca_sur %>% as.numeric() %>% sum,
    E = m/N
  ) %>% 
  ggplot(., aes(x = factor(Year), y = E)) +
  geom_boxplot()+
  scale_y_continuous(limits =  c(0, 0.1),
                     breaks = seq(0,0.1,0.02))+
  labs(x = "Year",
       y = "Encounter rate (troop/point)")+
  theme_classic()+
  theme(
    panel.border = element_blank()
  )



M.data %>% 
  group_by(Office, Survey) %>% 
  summarise(
    N = n(),
    m = Macaca_sur %>% as.numeric() %>% sum,
    E = m/N
  ) %>% 
  ggplot(., aes(x = factor(Office), y = E)) +
  geom_boxplot()+
  scale_y_continuous(limits =  c(0, 0.12),
                     breaks = seq(0,0.12,0.02))+
  labs(x = "Year",
       y = "Encounter rate (troop/point)")+
  theme_classic()+
  theme(
    panel.border = element_blank()
  )

M.data %>% 
  group_by(TypeName.1, Survey) %>% 
  summarise(
    N = n(),
    m = Macaca_sur %>% as.numeric() %>% sum,
    E = m/N
  ) %>% 
  ggplot(., aes(x = factor(TypeName.1), y = E)) +
  geom_boxplot()+
  scale_y_continuous(limits =  c(0, 0.06),
                     breaks = seq(0,0.06,0.02))+
  labs(x = "Year",
       y = "Encounter rate (troop/point)")+
  theme_classic()


emmip(m1, TypeName.1 ~ Office)
