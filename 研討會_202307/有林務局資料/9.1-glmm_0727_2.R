# Farmland (include orchard )

library(tidyverse)
library(readxl)
library(writexl)
library(here)


here::here() 


#-------------------------------------------

site_list <- 
  read_xlsx("./研討會_202307/data/refer/分層隨機取樣的樣區清單 _20221031.xlsx",
            sheet = "樣區表") %>% 
  .[,7] %>% 
  setNames(., "site_list")


BBS_Monkey_1522_GLMM <- 
  read.csv("./研討會_202307/有林務局資料/data/clean/BBS_Monkey_1522_0710.csv") %>% 
  reshape2::melt(id = "site", variable.name ="year", value.name = "count") %>% 
  mutate(year = str_remove_all(year, "X")) %>% 
  setNames(., str_replace_all(colnames(.), "site", "transect")) %>% 
  filter(transect %in% site_list$`site_list`)  %>% 
  mutate(count = ifelse(count %in% c(1:4), 1, 0))



# Farmland (include orchard )
Ch5_matrix_line_site <- 
  read_xlsx("./研討會_202307/有林務局資料/data/clean/BBSsite100mbuffer_0714.xlsx")

#----------------------------------------------

Df <- 
  
  BBS_Monkey_1522_GLMM %>% 
  left_join(Ch5_matrix_line_site, by = c("transect")) %>% 
  
  group_split(year) %>% 
  map(., function(x){
    
    
    sample(1:nrow(x), size = nrow(x[x$count  %in% 1,]), replace = F) %>% 
      slice(x, .) %>% 
      mutate(count = 0) 
    
  }) %>% 
  bind_rows() %>% 
  
  bind_rows(
    BBS_Monkey_1522_GLMM %>% 
      left_join(Ch5_matrix_line_site, by = c("transect")) %>% 
      filter(! is.na(Shannon))%>% 
      filter( count != 0)
  ) %>% 
  select(transect,year, count, elevation,edge,P_forest,P_farmland,Shannon,edge_length) %>% 
  
  mutate(year = year %>% as.numeric(.) %>% as.integer(.))%>% 
  mutate_at(c("elevation","edge","P_forest",
              "P_farmland", "Shannon", "edge_length"),scale) %>% 
  data.frame(.) 

#write.csv(Df, "./研討會_202307/有林務局資料/data/clean/Df_site_A-C_1522_0727_2.csv", row.names = F)

 Df <-
 read.csv("./研討會_202307/有林務局資料/data/clean/Df_site_A-C_1522_0727_2.csv")

#-------------------------
library(ggcorrplot)


corr <-
  Df %>% 
  na.exclude(.) %>%
  select(year,elevation,edge,P_forest,P_forest,P_farmland) %>%
  setNames(., str_remove_all(colnames(.), "P_")) %>% 
  setNames(., str_to_title(colnames(.))) %>% 
  cor(.) %>% 
  round(1)



ggcorrplot(corr, 
           #     method = "circle",
           # hc.order = TRUE,
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"),
           legend.title = "Pearson\nCorrelation",
           type = "lower",
           lab_size = 6,
           tl.cex = 12,
           tl.srt = 270,
           lab = TRUE)+ #數字為correlation coefficient
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    legend.position = c(0.3,0.9),
    legend.direction = "horizontal",
    axis.text.x = element_text(hjust = 0, vjust = 0.5)
  )+
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))


#------------------------------------
library(car)

vif(lm( count ~ year+ elevation+ edge + P_forest + P_farmland , Df))



#----------------------------------------------



library(glmmTMB)


model2 <- glmmTMB(
  count ~  elevation + edge + P_forest + P_farmland  + (1|year)+(1|transect) ,
  Df,
  family = binomial(link = "logit")  ) 



summary(model2)


glmmTMB(
  count ~  elevation + edge + P_forest + P_farmland  + (1|year)+(1|transect) ,
  Df,
  family = binomial(link = "logit")  ) %>% summary

#---------------------------------------------------

ggplot(Df, aes(x = P_forest, y = count))+
  geom_point(pch = 21, size = 2.5, col = "#1D3557", fill = "#457b9d")+
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              col = "red",
              se = F) + 
    scale_x_continuous(name ="Forest")+
  scale_y_continuous(name ="",limits = c(0, 1), breaks =c(0,1))+
  theme_classic()+
  theme(
    aspect.ratio = 2,
    text = element_text(size = 14)
    
  )
ggplot(Df, aes(x = P_farmland, y = count))+
  geom_point(pch = 21, size = 2.5, col = "#1D3557", fill = "#457b9d")+
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              col = "red",
              se = F) + 
    scale_x_continuous(name ="Farmland")+
  scale_y_continuous(name ="",limits = c(0, 1), breaks =c(0,1))+
  theme_classic()+
  theme(
    aspect.ratio = 2,
    text = element_text(size = 14)
    
  )

ggplot(Df, aes(x = edge, y = count))+
  geom_point(pch = 21, size = 2.5, col = "#1D3557", fill = "#457b9d")+
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              col = "red",
              se = F) + 
  scale_x_continuous(name ="Edge")+
  scale_y_continuous(name ="",limits = c(0, 1), breaks =c(0,1))+
  theme_classic()+
  theme(
    aspect.ratio = 2,
    text = element_text(size = 14)
    
  )

ggplot(Df, aes(x = elevation, y = count))+
  geom_point(pch = 21, size = 5.5, col = "#4a7c59", fill = "#8FC0A9", alpha = 1)+
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              col = "red",
              se = F) + 
  scale_x_continuous(name ="Elevation")+
  scale_y_continuous(name ="",limits = c(0, 1), breaks =c(0,1))+
  theme_classic()+
  theme(
    aspect.ratio = 1.65,
    text = element_text(size = 22)
    
  )

#----------------------
library(see)
BBS_Monkey_1522_GLMM %>% 
  left_join(Ch5_matrix_line_site, by = c("transect")) %>% 
  
  ggplot(., aes(x = count, y = elevation)) +
  geom_point(position = position_jitter(width = 0.05), alpha = .2)+
  geom_violinhalf(aes(col = as.character(count)), alpha = .1, show.legend = F)



#-----------------------------------------------------------------------
Df2 <- 
Df %>% 
  select(transect:count) %>% 
  mutate(year  = year %>% as.character) %>% 
  left_join(BBS_Monkey_1522_GLMM[,c("transect", "year")]) %>% 
  left_join(Ch5_matrix_line_site, by = c("transect")) %>% 
  mutate(year  = year %>% as.integer) 

##

ggplot(Df2, aes(x = P_forest, y = count))+
  geom_point(pch = 21, size = 5.5, col = "#0D2818", fill = "#629460", alpha = .6)+
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              col = "#FB6376",
              size = 2.5,
              se = F) + 
  scale_x_continuous(name ="Forest")+
  scale_y_continuous(name ="",limits = c(0, 1), breaks =c(0,1))+
  theme_classic()+
  theme(
    aspect.ratio = 1.65,
    text = element_text(size = 22)
    
  )
ggplot(Df2, aes(x = P_farmland, y = count))+
  geom_point(pch = 21, size = 5.5, col = "#0D2818", fill = "#629460", alpha = .6)+
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              col = "#FB6376",
              size = 2.5,
              se = F) + 
  scale_x_continuous(name ="Farmland")+
  scale_y_continuous(name ="",limits = c(0, 1), breaks =c(0,1))+
  theme_classic()+
  theme(
    aspect.ratio = 1.65,
    text = element_text(size = 22)
    
  )

ggplot(Df2, aes(x = edge, y = count))+
  geom_point(pch = 21, size = 5.5, col = "#0D2818", fill = "#629460", alpha = .6)+
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              col = "#FB6376",
              size = 2.5,
              se = F) + 
  scale_x_continuous(name ="Edge")+
  scale_y_continuous(name ="",limits = c(0, 1), breaks =c(0,1))+
  theme_classic()+
  theme(
    aspect.ratio = 1.65,
    text = element_text(size = 22)
    
  )




## 
ggplot(Df2, aes(x = elevation, y = count))+
  geom_point(pch = 21, size = 5.5, col = "#0D2818", fill = "#629460", alpha = .6)+
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              col = "#FB6376",
              size = 2.5,
              se = F) + 
  scale_x_continuous(name ="Elevation")+
  scale_y_continuous(name ="",limits = c(0, 1), breaks =c(0,1))+
  theme_classic()+
  theme(
    aspect.ratio = 1.65,
    text = element_text(size = 22)
    
  )

#--------------------------------

ELE_cut <- 
read.csv("./研討會_202307/有林務局資料/data/clean/BBS_Monkey_1522_0710.csv") %>% 
  reshape2::melt(id = "site", variable.name ="year", value.name = "count") %>% 
  mutate(year = str_remove_all(year, "X")) %>% 
  setNames(., str_replace_all(colnames(.), "site", "transect")) %>% 
  filter(transect %in% site_list$`site_list`)  %>% 
#  mutate(count = ifelse(count %in% c(1:4), 1, count))%>% 
  
  
  left_join(Ch5_matrix_line_site, by = c("transect"))%>% 
  mutate(ELE_f = cut(elevation,
                     breaks = c(seq(0,4000,500)),
               #      labels = c(seq(100,3900,200)),
                     include.lowest = T) ) 

Text_site <- 
ELE_cut %>% 
  filter(!is.na(count)) %>% 
  group_by(ELE_f, year) %>%
  summarise(E = sum(count, na.rm = T)/n(),
            n = n()) %>%
  ungroup() %>% 
  
  group_by(ELE_f) %>%
  summarise(site_mean = mean(n) %>% round(1),
            E_q3 = quantile(E,0.75)) %>%
  ungroup() 
  


ELE_cut %>% 
  filter(!is.na(count)) %>% 
  group_by(ELE_f, year) %>%
  summarise(E = sum(count, na.rm = T)/n(),
            n = n()) %>%
  ungroup() %>% 

  ggplot(., aes(y = E, x = ELE_f)) +
  # geom_violinhalf( alpha = .1, show.legend = F)+
  geom_boxplot(width = .5 ,fatten= 3 ,alpha = .9)+
  #  geom_point(position = position_jitter(width = .4), alpha = .2)+
  geom_text(data = Text_site, 
            aes(y = E_q3,
                label = site_mean),
            size = 3.7,
            hjust = -.2, 
            vjust = -.3,
            position = position_dodge(0.9), show.legend = F)+
  
  scale_x_discrete(name ="Elevation (m)", labels = c("250","750",
                                                     "1250","1750",
                                                     "2250","2750",
                                                     "3250","3750"))+
  scale_y_continuous(name ="Encounter rate (troops/transect)" )+
  theme_bw()+
  theme(
    aspect.ratio = 1.0,
    text = element_text(size = 20),
    line = element_line(linewidth = .9),
    axis.title.y = element_text(angle = 270),
    panel.grid = element_blank()
    
  )
  
