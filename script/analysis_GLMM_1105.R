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
                     "台東縣","臺東縣"), Region := "East"] %>%
  .[  Region %in% "East", Region.2 := "East"] %>%
  .[!(Region %in% "East"), Region.2 := "Wast"] %>%
  .[  Region %in% "North", Region.3 := "North"] %>%
  .[ Region %in% "South", Region.3 := "South"]  %>%
  .[ Region %in% "Center", Region.3 := "South"] %>%
  .[ Region %in% "East", Region.3 := "South"] %>%
  .[Year <2019,]



#========================================================
M.data$Year %<>% as.numeric
M.data$Survey %<>% as.factor
M.data$Region %<>% as.factor
M.data$Region.2 %<>% as.factor
M.data$Region.3 %<>% as.factor



m1 <- glmer(Macaca_sur ~ TypeName.1 + Altitude +  Survey + Year.re + Region.2  + (1|Site_N), 
            family = binomial, data = M.data)
Anova(m1)


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

#=====================
tt<- M.data %>% setDT %>% .[is.na(Macaca_sur),Macaca_sur:=0 ] %>%
  .[!is.na(TypeName.1), .(Mean = mean(Macaca_sur, na.rm=T) ,
                          Var = var(Macaca_sur, na.rm=T),
                          Length = length(Macaca_sur)),
    by = list(TypeName.1, Altitude,  Region.2 , Survey )] %>%
  .[, N := sum(Length)]
  
