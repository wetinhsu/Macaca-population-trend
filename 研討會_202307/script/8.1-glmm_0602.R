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
  read.csv("./研討會_202307/data/clean/BBS_Monkey_1522_0524.csv") %>% 
  reshape2::melt(id = "site", variable.name ="year", value.name = "count") %>% 
  mutate(year = str_remove_all(year, "X")) %>% 
  setNames(., str_replace_all(colnames(.), "site", "transect")) %>% 
  filter(transect %in% site_list$`site_list`)  %>% 
  mutate(count = ifelse(count %in% c(1:4), 1, 0))


Ch5_matrix_line_site <- 
  read_xlsx("./研討會_202307/data/clean/BBSsite100mbuffer_0602.xlsx")

#----------------------------------------------

Df <- 
  
  BBS_Monkey_1522_GLMM %>% 
  left_join(Ch5_matrix_line_site, by = c("transect")) %>% 
  filter(! is.na(Shannon))%>% 
  
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



write.csv(Df, "./研討會_202307/data/clean/Df_site_A-C_1522_0602.csv", row.names = F)
#----------------------------------
library(car)

vif(lm( count ~ year+ elevation+ edge + P_forest + P_farmland + Shannon  , Df))



#-------------------------
library(ggcorrplot)

corr <- 
  Df %>% 
  filter(!is.na(edge)&!is.na(P_forest)&!is.na(P_farmland)&!is.na(Shannon)&!is.na(elevation)) %>% 
  select(year,elevation,edge,P_forest,P_farmland,Shannon) %>% 
  cor(.) %>% 
  round(1)


corr <-
  Df %>% 
  na.exclude(.) %>%
  select(year,elevation,edge,P_forest,P_farmland,Shannon) %>%
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
  


#----------------------------------------------



library(glmmTMB)


model2 <- glmmTMB(
  count ~  elevation + edge + P_forest + P_farmland + 
    Shannon  + (1|year)+(1|transect) ,
  Df,
  family = binomial(link = "logit")  )



summary(model2)




#---------------------------------------------------

ggplot(Df, aes(x = Shannon, y = count))+
  geom_point(pch = 21, size = 2.5, col = "#1D3557", fill = "#457b9d")+
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              col = "red",
              se = F) + 
#  scale_x_continuous(name ="Shannon",limits = c(-1.9, 1.3))+
  scale_y_continuous(name ="",limits = c(0, 1), breaks =c(0,1))+
  theme_classic()+
  theme(
    aspect.ratio = 2,
    text = element_text(size = 14)
    
  )


Df <- 
  read.csv("./研討會_202307/data/clean/Df_site_A-C_1522_0602.csv")
