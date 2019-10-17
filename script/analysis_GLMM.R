# data analysis

library(data.table)
library(lme4)
library(car)
library(magrittr)
library(multcomp)
library(ggplot2)
library(psych)
library(readxl)

M.data <- 
  read_xlsx("data/clean/data_for_analysis.xlsx") %>% 
  setDT %>% 
  .[, Year := as.numeric(Year)] %>% 
  .[, Year.re := Year - min(Year) + 1] %>%
  .[, high := substr(Site_N, 1, 1)] %>%
  .[County %in% list("基隆市","台北市","臺北市",
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
  .[County %in% list("宜蘭縣","花蓮縣",
                     "台東縣","臺東縣"), Region := "East"]

# M.data$Year %<>% as.numeric
# M.data$Survey %<>% as.factor
# m0 <- glmer(Macaca_sur ~ TypeName * Year + (1|Site_N), 
#             family = binomial, data = M.data)

m1 <- glmer(Macaca_sur ~ Year.re  + Region +(1|Site_N), 
            family = binomial, data = M.data)

Anova(m1)
summary(glht(m1, linfct = mcp(TypeName = "Tukey")))
summary(glht(m1, linfct = mcp(Region = "Tukey")))
summary(glht(m1, linfct = mcp(Year = "Tukey")))

### Summary
M.data[, .(M = sum(Macaca_sur)), by = list(Year, Survey, TypeName)] %>% 
  dcast(Year + Survey ~ TypeName, value.var = "M")

M.data[, .(Mean = round(mean(Macaca_sur), 3),
           SD = round(sd(Macaca_sur), 3)),
       by = list(Year, TypeName)] %>% 
  dcast(Year ~ TypeName, value.var = c("Mean", "SD"))

M.data[, .(Mean = mean(Macaca_sur),
           SD = sd(Macaca_sur)/sqrt(length(Macaca_sur))), 
       by = Year]

# describeBy(M.data$Macaca_sur, 
#            group = M.data[,c("TypeName", "Year")])
