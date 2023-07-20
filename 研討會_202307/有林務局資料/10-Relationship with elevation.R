# Farmland (include orchard )

library(tidyverse)
library(readxl)
library(writexl)
library(here)


here::here() 

#------------------------------
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
  filter(transect %in% site_list$`site_list`) # %>% 
 # mutate(count = ifelse(count %in% c(1:4), 1, 0))



# Farmland (include orchard )
Ch5_matrix_line_site <- 
  read_xlsx("./研討會_202307/有林務局資料/data/clean/BBSsite100mbuffer_0714.xlsx") %>% 
  select(transect,elevation)



#--------------------------------
BBS_Monkey_1522_GLMM %>% 
  left_join(Ch5_matrix_line_site, by = c("transect"))%>% 
  mutate(ELE_f = cut(elevation,
                     breaks = c(seq(0,4000,500)),
                     #      labels = c(seq(100,3900,200)),
                     include.lowest = T) ) %>% 
  group_by(ELE_f, year) %>%
  summarise(E = sum(count, na.rm = T)/n()) %>%
  ungroup() %>%
  
  
  ggplot(., aes(y = E, x = ELE_f)) +
  # geom_violinhalf( alpha = .1, show.legend = F)+
  geom_boxplot(width = .5 ,fatten= 3 ,alpha = .9)+
  #  geom_point(position = position_jitter(width = .4), alpha = .2)+
  scale_x_discrete(name ="Elevation (m)", labels = c("250","750",
                                                     "1250","1750",
                                                     "2250","2750",
                                                     "3250","3750"))+
  scale_y_continuous(name ="Encounter rate (troop/transect)" )+
  theme_bw()+
  theme(
    aspect.ratio = 1.0,
    text = element_text(size = 20),
    line = element_line(linewidth = .9),
    axis.title.y = element_text(angle = 270),
    panel.grid = element_blank()
    
  )

