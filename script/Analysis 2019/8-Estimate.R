
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
  .[analysis %in% "Y",] %>% 
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


replicate(10000, mean(sample(bb$AB, replace = TRUE))) %>%
  quantile(.,probs = c(0.025, 0.975)) 

replicate(10000, mean(sample(bb$AB, replace = TRUE))) %>% mean

21028.14/(0.1*0.1*pi)


plot(5000,0.3,xlim = c(1005,1500), ylim = c(0.01,0.02),type = "n")
tmp.M <- c()
Down <- c()
Up <- c()


for(i in seq(1005,1500,5)){
  tmp <- replicate(i, mean(sample(bb$AB, replace = TRUE))) 
  
  tmp.M <- mean(tmp) %>% c(tmp.M, .)
  Down <- quantile(tmp,probs = c(0.025)) %>% as.numeric() %>% c(Down, .)
  Up <- quantile(tmp,probs = c(0.975)) %>% as.numeric() %>% c(Up, .)
  
}
#ggplot()+geom_errorbar(aes(x=2, ymin = Down, ymax=Up), width=0.3)

points(c(seq(1005,1500,5)),tmp.M,pch=".",type = "l")
points(c(seq(1005,1500,5)), Up, pch=".", col="blue",type = "l")
points(c(seq(1005,1500,5)),Down, pch=".", col="blue",type = "l")



#獨立樣本
df %>% 
  .[, .(E = sum(Macaca_sur)/length(Macaca_sur)), by = c("Year", "Survey")] %>% 
  .[ ,list(mean = mean(E)/(0.1*0.1*pi) , sd = sd(E)/sqrt(10)/(0.1*0.1*pi))] 
  
0.5-0.04*1.96
0.5+0.04*1.96

#檢測是否符合常態
test <- 
df %>% 
  .[, .(E = sum(Macaca_sur)/length(Macaca_sur)), by = c("Year", "Survey")] 
  qqnorm(test$E)
  qqline(test$E)
  
  shapiro.test(test$E)
  

  
#分層抽樣"Year", "Survey",

df %>% 
  .[, .(E.mean = mean(Macaca_sur),
        E.var = var(Macaca_sur),
        n = length(Macaca_sur)), by = c( "Region2")] %>% 
  .[ , list(MEAN = mean(E.mean),
            VAR = sum((sum(n)-n)*(E.var/n))/sum(n))] 


0.02624293/(0.1*0.1*pi)

0.0001313601-(1.96*(0.01283338^0.5))
0.0001313601+(1.96*(0.01283338^0.5))

-0.2219063/(0.1*0.1*pi)
0.222169/(0.1*0.1*pi)




