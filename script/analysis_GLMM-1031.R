# data analysis

library(data.table)
library(lme4)
library(car)
library(magrittr)
library(multcomp)
library(ggplot2)
library(psych)
library(readxl)
library(asbio)

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
                     "台東縣","臺東縣"), Region := "East"] 


#========================================================
 M.data$Year %<>% as.numeric
 M.data$Survey %<>% as.factor
 
 summary(M.data)
 

m1 <- glmer(Macaca_sur ~ Year.re  + TypeName.1  + factor(Survey) + Altitude + (1|Site_N), 
            family = binomial, data = M.data)
Anova(m1)


#full model Region +
#Macaca_sur ~ Year.re  + TypeName.1  + factor(Survey) + Altitude + Region + (1|Site_N)



m0 <- glm(Macaca_sur ~ Year.re  + TypeName.1  + factor(Survey) + Altitude + Region + Region, 
      family = binomial, data = M.data)


m01.1 <- update(m0, ~ .+ factor(Survey)) ; anova(m01.1)
m01.1 <- update(m0, ~ .+ Altitude) ; anova(m01.1)
m01.1 <- update(m0, ~ .) ; anova(m01.1)


mm<- stepAIC(m0,trac=F)
summary(mm)
Anova(mm)
