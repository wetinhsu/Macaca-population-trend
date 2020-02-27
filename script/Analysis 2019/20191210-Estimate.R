
library(data.table)
library(car)
library(magrittr)
library(multcomp)
library(ggplot2)
library(readxl)

#------------------------------------------------

#Original data---- 

M.data <- read_excel("./data/clean/for analysis_V1.xlsx",
                     sheet=1) %>% setDT %>% 
  .[TypeName %like% "混", TypeName.n := "mixed"] %>% 
  .[TypeName %like% "竹林", TypeName.n := "Bamboo"] %>% 
  .[TypeName %like% "闊葉", TypeName.n := "broad-leaved"] %>% 
  .[TypeName %like% "針葉", TypeName.n := "coniferous"] %>% 
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


#---------------------------------------------------------------------

M.data <- M.data %>% 
  .[is.na(Macaca_sur), Macaca_sur := 0] %>% 
  .[, Year := as.numeric(Year)] %>% 
  .[, Year.re := Year - min(Year) + 1]



#==============================================
df <- 
  M.data %>% 
  #.[Year < 2019,] %>%
  .[!(TypeName.1 %in% "Not forest"), ] 

#Estimate==============================================
#bootstrap-------------------------
bb<- df %>% setDT %>% 
  .[, A := ifelse(Macaca_dist %in% "A", Macaca_sur,0)] %>% 
  .[, AB := ifelse(Macaca_dist %in% c("A","B"), Macaca_sur,0)] 


replicate(5000, mean(sample(bb$AB, replace = TRUE))) %>%
  quantile(.,probs = c(0.025, 0.975)) 

replicate(5000, mean(sample(bb$AB, replace = TRUE))) %>% mean

21536.41/(0.1*0.1*pi)



replicate(5000, mean(sample(bb$A, replace = TRUE))) %>%
  quantile(.,probs = c(0.025, 0.975))

replicate(5000, mean(sample(bb$A, replace = TRUE))) %>% mean

21536.41/(0.025*0.025*pi)




#level-------------------------------
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


