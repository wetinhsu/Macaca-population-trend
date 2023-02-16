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


Ch5_multiSTDq <- 
  read.csv("./研討會_202307/data/refer/Dali/Ch5_multiSTDq.csv") %>% 
  select(transect, elevation_mean,area, ends_with("500"))


BBS_Monkey_1521_GLMM <- 
  read.csv("./研討會_202307/data/clean/BBS_Monkey_1521_0214.csv") %>% 
  reshape2::melt(id = "site", variable.name ="year", value.name = "count") %>% 
  mutate(year = str_remove_all(year, "X")) 
  
Ch5_matrix_line_site <- 
  read_xlsx("./研討會_202307/data/refer/Dali/Ch5_matrix_line_site.xlsx")%>% 
  mutate(transect = str_sub(site, 1,6), .before = site) %>% 
  group_by(transect) %>% 
  summarise(elevation_mean  = mean(elevation, na.rm = T),
            forest  = sum(`forest...17`, na.rm = T),
            farmland  = sum(`farmland...18`, na.rm = T),
            water  = sum(water , na.rm = T),
            building  = sum(building, na.rm = T),
            other  = sum(`other...21`, na.rm = T),
            edge_length  = sum(edge_length, na.rm = T)
  ) %>% 
  
  mutate(total = forest+farmland+water+building+other) %>% 
  
  mutate(P_forest = forest/total,
         P_farmland = farmland/total,
         P_water = forest/total,
         P_building = building/total,
         P_other = other/total
  )  

Ch5_multiSTDq_2 <- 
  Ch5_matrix_line_site %>% 
  bind_cols(
  Ch5_matrix_line_site %>% 
    select(P_forest, P_farmland, P_water, P_building,P_other) %>% 
    mutate(P_forest = ifelse(P_forest ==0 , NA, P_forest),
           P_farmland = ifelse(P_farmland ==0 , NA, P_farmland),
           P_water = ifelse(P_water ==0 , NA, P_water),
           P_building = ifelse(P_building ==0 , NA, P_building),
           P_other = ifelse(P_other ==0 , NA, P_other)
    ) %>% 
    mutate(Shannon = sapply(., function(x){
      x*log(x) } ) %>%  
        apply(.,1,sum,na.rm = T)*(-1)
      ) %>% 
    select(Shannon)
  ) %>% 
  
  mutate(area = edge_length*1000/total) 
 
  
  
  Ch5_multiSTDq_2 %>% 
    filter(str_detect( transect , "B|C|(A04-02)|(A26-06)" , negate = T)) %>%  
  select(transect,elevation_mean,area,P_forest,
              P_farmland, Shannon, edge_length) %>% 
    mutate_at(c("elevation_mean","area","P_forest",
                "P_farmland", "Shannon", "edge_length"),scale) %>% 
    data.frame() %>% 
    setNames(., str_remove_all(colnames(.), "1"))


#------------------------------------------------------------------------

  
  BBS_Monkey_1521_GLMM %>% 
    slice(sample(1:nrow(BBS_Monkey_1521_GLMM), size = 161, replace = F)) %>% 
    mutate(count = 0) %>% 
    
    
    bind_rows(
BBS_Monkey_1521_GLMM %>% 
  filter(!(is.na(count)| count == 0))
        
      
    ) %>% 
  left_join(Ch5_multiSTDq_2, by = c("site" = "transect")) %>%
  View



    