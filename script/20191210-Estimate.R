
library(data.table)
library(car)
library(magrittr)
library(multcomp)
library(ggplot2)
library(readxl)

#------------------------------------------------

#Original data---- 

M.data <- read_excel("./data/clean/for analysis.xlsx",
                     sheet=1) %>% setDT %>% 
  .[, DATE := as.IDate(paste(Year, Month, Day, sep = "/"))] %>% 
  .[TypeName %like% "混", TypeName.n := "mixed"] %>% 
  .[TypeName %like% "竹林", TypeName.n := "Bamboo"] %>% 
  .[TypeName %like% "闊葉", TypeName.n := "broad-leaved"] %>% 
  .[TypeName %like% "針葉", TypeName.n := "coniferous"] %>% 
  .[, TypeName.1 := ifelse(Distance>20, "Not forest", TypeName.n)] %>% 
  .[, County := ordered(County,
                        c("宜蘭縣","基隆市","台北市","臺北市",
                          "新北市","台北縣","臺北縣",
                          "桃園縣","桃園市","新竹市",
                          "新竹縣","苗栗縣",
                          "台中市","臺中市",
                          "台中縣","臺中縣",
                          "彰化縣","南投縣","南投市",
                          "雲林縣","嘉義縣","嘉義市",
                          "台南市","臺南市",
                          "台南縣","臺南縣",
                          "高雄縣","高雄市",
                          "屏東縣", "花蓮縣",
                          "台東縣","臺東縣"))] %>% 
  
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
  .[, julian.D := yday(DATE)] %>% 
  .[, Altitude_c := substr(Site_N,1,1)] %>% setDT %>% 
  .[julian.D > 75 & julian.D <= 180, ]

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


#---------------------------------------------------------------------

M.data <- M.data %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>% 
  .[, Year := as.numeric(Year)] %>% 
  .[, Year.re := Year - min(Year) + 1]



#==============================================
df <- 
  M.data %>% 
  .[Year < 2019,] %>%
  .[!(TypeName.1 %in% "Not forest"), ] %>% 
  .[Macaca_dist %in% "c", Macaca_sur :=0]

#Estimate==============================================
##<25
aa<- df %>% setDT %>% 
  .[, A := ifelse(Macaca_dist %in% "A", Macaca_sur,0)] %>% 
  .[, AB := ifelse(Macaca_dist %in% c("A","B"), Macaca_sur,0)]  %>% 
  .[, .(Mean = mean(A, na.rm=T),
        SD = sd(A, na.rm=T)/sqrt(length(A)),
        n = .N), 
    by = list(TypeName.1, Region)] %>%
  .[, N:= sum(n)]

mean(aa$Mean)%>% round(.,7)
(var.25 <- sum(aa$N*(aa$N-aa$n)*(aa$SD)^2/aa$n, na.rm=T)/(unique(aa$N)^2))
var.25^0.5 %>% round(.,7)

mean(aa$Mean)-(var.25^0.5*1.28)%>% round(.,7)
mean(aa$Mean)+(var.25^0.5*1.28)%>% round(.,7)  #80%CI=se*1.28

mean(aa$Mean)-(var.25^0.5*1.96)%>% round(.,7)
mean(aa$Mean)+(var.25^0.5*1.96)%>% round(.,7)


21536.41/(0.025*0.025*pi)


##<100
bb<- df %>% setDT %>% 
  .[, A := ifelse(Macaca_dist %in% "A", Macaca_sur,0)] %>% 
  .[, AB := ifelse(Macaca_dist %in% c("A","B"), Macaca_sur,0)]  %>% 
  .[, .(Mean = mean(AB, na.rm=T),
        SD = sd(AB, na.rm=T)/sqrt(length(AB)),
        n = .N), 
    by = list(TypeName.1, Region)] %>%
  .[, N:= sum(n)]

mean(bb$Mean)%>% round(.,7)
(var.100 <- sum(bb$N*(bb$N-bb$n)*(bb$SD)^2/bb$n, na.rm=T)/(unique(bb$N)^2))
var.100^0.5%>% round(.,7)

mean(bb$Mean)-var.100^0.5*1.28%>% round(.,7)
mean(bb$Mean)+var.100^0.5*1.28%>% round(.,7)

mean(bb$Mean)-var.100^0.5*1.96%>% round(.,7)
mean(bb$Mean)+var.100^0.5*1.96%>% round(.,7)


21536.41/(0.1*0.1*pi)



#bootstrap-------------------------
bb<- df %>% setDT %>% 
  .[, A := ifelse(Macaca_dist %in% "A", Macaca_sur,0)] %>% 
  .[, AB := ifelse(Macaca_dist %in% c("A","B"), Macaca_sur,0)] 


replicate(10000, mean(sample(bb$AB, replace = TRUE))) %>%
         quantile(.,probs = c(0.025, 0.975))
  
replicate(10000, mean(sample(bb$AB, replace = TRUE))) %>% mean

21536.41/(0.1*0.1*pi)



replicate(10000, mean(sample(bb$A, replace = TRUE))) %>%
  quantile(.,probs = c(0.025, 0.975))

replicate(10000, mean(sample(bb$A, replace = TRUE))) %>% mean

21536.41/(0.025*0.025*pi)

