library(tidyverse)
library(readxl)
library(writexl)
library(here)


here::here() 


site_list <- 
  read_xlsx("./研討會_202307/data/refer/分層隨機取樣的樣區清單 _20221031.xlsx",
            sheet = "樣區表") %>% 
  .[,7] %>% 
  setNames(., "site_list")




BBS_Monkey_1521_GLMM <- 
  read.csv("./研討會_202307/data/clean/BBS_Monkey_1521_0214.csv") %>% 
  reshape2::melt(id = "site", variable.name ="year", value.name = "count") %>% 
  mutate(year = str_remove_all(year, "X")) 

Ch5_matrix_line_site <- 
  read_xlsx("./研討會_202307/data/refer/Dali/Ch5_matrix_line_site.xlsx")


M.data <- read_excel(here("./研討會_202307/data/clean/for analysis_1521_v2.xlsx"),
                     sheet=1) %>% 
  
  mutate_at(c("Year", "Survey", "Point", "Macaca_sur",
              "Month", "Day", "Altitude", "julian.D"), as.numeric) %>% 

  filter(Site_N %in% site_list$`site_list`) %>% 
  
  filter(!(Year %in% 2020 & Survey %in% 3)) %>% 
  filter(analysis=="Y") %>% 
  mutate(transect = paste0(Site_N, "-", Point))


#---------------------------------

Df <- 

M.data %>% 
  select(-(Macaca_dist:analysis))%>%
  left_join(Ch5_matrix_line_site, by = c("transect" = "site")) %>% 
  filter(! is.na(Shannon))%>% 
  
  
  group_split(Year) %>% 
  map(., function(x){
    
    
      sample(1:nrow(x), size = nrow(x[x$Macaca_sur %in% 1,]), replace = F) %>% 
      slice(x, .) %>% 
      mutate(Macaca_sur = 0) 
    
  }) %>% 
  bind_rows() %>% 
 
  bind_rows(
    M.data %>% 
      select(-(Macaca_dist:analysis))%>%
      left_join(Ch5_matrix_line_site, by = c("transect" = "site")) %>% 
      filter(! is.na(Shannon))%>% 
      filter( Macaca_sur != 0)
  ) %>% 
  select(transect,Site_N,Year, Macaca_sur, elevation,edge,P_forest,P_farmland,Shannon,edge_length) %>% 
  
  mutate(Year = Year %>% as.numeric(.) %>% as.integer(.))%>% 
  mutate_at(c("elevation","edge","P_forest",
              "P_farmland", "Shannon", "edge_length"),scale) %>% 
  data.frame(.) 






#----------------------------------
library(car)

vif(lm( Macaca_sur ~ Year+ elevation+ edge + P_forest + P_farmland +
    Shannon  , Df))

#-------------------------
library(ggcorrplot)

corr <- 
  Df %>% 
  filter(!is.na(edge)) %>% 
  select(Year,elevation,edge,P_forest,P_farmland,Shannon) %>% 
  cor(.) %>% 
  round(1)



ggcorrplot(corr, 
          # method = "circle",
           hc.order = TRUE,
           type = "lower",
           lab = TRUE) #數字為correlation coefficient



#----------------------------------------------



library(glmmTMB)


model2 <- glmmTMB(
  Macaca_sur ~  elevation+ edge + P_forest + P_farmland +
    Shannon  + (1|Year)+(1|transect) ,
  Df,
  family = binomial(link = "logit") )



summary(model2)

write.csv(Df,"./研討會_202307/data/clean/Df_0222.csv",
          row.names = F)

Df <- read.csv("./研討會_202307/data/clean/Df_0222.csv" )%>% 
  mutate_at(c("elevation","edge","P_forest",
              "P_farmland", "Shannon", "edge_length"),scale)
#--------------------

ggplot(Df, aes(x = P_farmland, y = Macaca_sur))+
  geom_point(pch = 1)+
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = F)
