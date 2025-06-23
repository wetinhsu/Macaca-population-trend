library(tidyverse)
library(readxl)
library(glmmTMB)
#-------------------------------------------------
# input filenames
dt_file <- "2025林業署訓練班測驗.xlsx"

# read data by sheet
dt <- 
  c("前測","後測", "解答") %>% 
  map(., function(x){
    
    x %>% 
      read_excel( path = dt_file, sheet = ., col_names = T) 
  }) %>% 
  setNames(., c("前測","後測", "解答"))

#read data
dt$`基本資料` <- 
  read_excel( path = dt_file, sheet = "基本資料", col_names = T) 

dt$`題目` <- 
  read_excel( path = dt_file, sheet = "題目", col_names = T)

Answer <- 
  dt$`解答` %>% as.list() %>% 
    map(function(x){ str_split(x, "、") %>% unlist 
      })


# 同時做前後測的人
all_day <- 
  inner_join(dt$`前測`, dt$`後測`, by = "人名") %>% 
  select(人名)

# 統計前後測人數
person <- 
  list(前測 = dt$`前測`, 後測 =  dt$`後測`, Both = all_day) %>% 
  map(., nrow) %>% bind_rows(.id = "A")


# 對答案並篩選出完整測驗的資料

DT_2025 <- 
dt[1:2]%>% #前測、後測
  map(mutate_all, as.character) %>% 
  map(function(y){
    y %>% 
      split(., .$人名)  %>%  #細分成一人名一筆
      map(function(z){
        
        tmp_1 <- 
          z %>% 
          .[2:31] %>% 
          str_split(., "、") %>% 
          map2(Answer[c(2:31)], ., function(x,y){ 
            ifelse( x %in% y, 1,0) 
          }) %>% 
          map(function(x){ 
            x %>%
              .[x ==1] %>%
              length %>% 
              ifelse(.==2, 1,.)
            })%>%
          bind_cols() 
        
        
        tmp_2 <- 
        z %>% 
          .[32:33] %>% 
          str_split(., "、") %>% 
          map2(Answer[c(32:33)], ., function(x,y){ 
            ifelse( x %in% y, x,0) 
          }) %>% 
           map(function(x) paste0(x, collapse = "、") )%>%
           bind_cols() 
        
        cbind(tmp_1, tmp_2)
        
      })%>%
      bind_rows(.id = "人名") 
    
    })%>% 
  bind_rows(.id = "Test") %>% 


  separate(., col = "綜合1",
           into = paste0("綜合1", sep = "_", Answer$綜合1), sep = "、") %>% 
  separate(., col = "綜合2",
           into = paste0("綜合2", sep = "_", Answer$綜合2), sep = "、") %>% 
  mutate_at(names(.)[33:ncol(.)], function(x){
    ifelse(x != 0, 1,0)
  })%>%
  
  mutate(score_綜合 = (
    case_when(        #對一個1分，對兩個3分，對三個5分
      apply(select(.,starts_with("綜合1")),1,sum, na.rm = T) == 3 ~ 5,
      apply(select(.,starts_with("綜合1")),1,sum, na.rm = T) == 2 ~ 3,
      apply(select(.,starts_with("綜合1")),1,sum, na.rm = T) == 1 ~ 1,
      apply(select(.,starts_with("綜合1")),1,sum, na.rm = T) == 0 ~ 0
    )+ 
      case_when(        #對一個1分，對兩個3分，對三個5分
        apply(select(.,starts_with("綜合2")),1,sum, na.rm = T) == 3 ~ 5,
        apply(select(.,starts_with("綜合2")),1,sum, na.rm = T) == 2 ~ 3,
        apply(select(.,starts_with("綜合2")),1,sum, na.rm = T) == 1 ~ 1,
        apply(select(.,starts_with("綜合2")),1,sum, na.rm = T) == 0 ~ 0
        )
    )) %>%   #算答對的題數
  
  mutate(score_方法 = 3*apply(select(.,starts_with("方法")),1,sum, na.rm = T)) %>%
  mutate(score_照片 = 3*apply(select(.,starts_with("照片")),1,sum, na.rm = T)) %>%
  mutate(score_聲音 = 3*apply(select(.,starts_with("聲音")),1,sum, na.rm = T)) %>%
  mutate(總分 = score_方法 + score_照片 + score_聲音 + score_綜合) #%>% 
 # setNames(., str_replace_all(colnames(.), set_names(dt$題目$考題, dt$題目$題目)))

View(DT_2025,"DT_2025")



 
DT.1 <- 
DT_2025  %>% 
  right_join(dt$`基本資料`,., by = c("姓名" = "人名")) %>% 
  filter(姓名 %in% all_day$人名 )%>% 
  mutate(身分 = str_replace_all(身分,c( ".*志工" = "志工",
                                   # "臨時人員" = "職員",
                                    "技術士|臨時人員|職員" = "森林護管員",
                                    
                                    "^阿里山生態.*" = "志工") )) %>% 
  mutate(調查 = case_when(
    是否執行過臺灣獼猴和繁殖鳥類調查 %in% c("只有執行過獼猴調查","都未曾執行過") ~ "NoBird",
    是否執行過臺灣獼猴和繁殖鳥類調查 %in% c("只有執行過繁殖鳥類調查","臺灣獼猴和繁殖鳥類調查都有執行過") ~ "Bird"

  )) 

DT.1$是否執行過臺灣獼猴和繁殖鳥類調查 %>% table
DT.1$調查 %>% table
DT.1$身分 %>% table





# 分析
model_total <- 
  glmmTMB(總分 ~  Test +  調查 + (1|姓名), 
          data = DT.1, family = gaussian)

analysis_summary <- 
  summary(model_total)$coefficients$cond %>% as.data.frame()

car::Anova(model_total)


ggplot(DT.1, aes(調查, 總分))+
  geom_boxplot(aes(fill = Test))


#---
model_mothod <- 
  glmmTMB(score_方法 ~  Test +  調查 + (1|姓名), 
          data = DT.1, family = gaussian)

analysis_summary <- 
  summary(model_mothod)$coefficients$cond %>% as.data.frame()

car::Anova(model_mothod)


ggplot(DT.1, aes(調查, score_方法))+
  geom_boxplot(aes(fill = Test))


#----
model_picture <- 
  glmmTMB(score_照片 ~  Test +  調查 + (1|姓名), 
          data = DT.1, family = gaussian)

analysis_summary <- 
  summary(model_picture)$coefficients$cond %>% as.data.frame()

car::Anova(model_picture)

ggplot(DT.1, aes(調查, score_照片))+
  geom_boxplot(aes(fill = Test))




#----
model_sound <- 
  glmmTMB(score_聲音 ~  Test +  調查 + (1|姓名), 
          data = DT.1, family = gaussian)

analysis_summary <- 
  summary(model_sound)$coefficients$cond %>% as.data.frame()

car::Anova(model_sound)


ggplot(DT.1, aes(調查, score_聲音))+
  geom_boxplot(aes(fill = Test))





#---
model_綜合 <- 
  glmmTMB(score_綜合 ~  Test +  調查 + (1|姓名), 
          data = DT.1, family = gaussian)

analysis_summary <- 
  summary(model_綜合)$coefficients$cond %>% as.data.frame()

car::Anova(model_綜合)


ggplot(DT.1, aes(調查, score_綜合))+
  geom_boxplot(aes(fill = Test))



#---------------
all_T.test <- 
paste0("方法", sprintf("%02.f",1:10)) %>% 
  lapply(function(x){
    
    DT.1 %>% 
      dplyr::select(姓名,Test, x) %>% 
      pivot_wider(names_from =Test, values_from = c(x))%>% 
      
      t.test(Pair(`前測`, `後測`) ~1, data = .)%>%
      .[c(1,3)] %>% bind_cols
    
  })%>% bind_rows()%>% 
  mutate(tracect = paste0("方法", sprintf("%02.f",1:10)), .before = 'statistic') %>% 
  
  bind_rows(

paste0("照片", sprintf("%02.f",1:10)) %>% 
  lapply(function(x){
    
    DT.1 %>% 
      dplyr::select(姓名,Test, x) %>% 
      pivot_wider(names_from =Test, values_from = c(x))%>% 
      
      t.test(Pair(`前測`, `後測`) ~1, data = .)%>%
      .[c(1,3)] %>% bind_cols
    
  })%>% bind_rows()%>% 
  mutate(tracect = paste0("照片", sprintf("%02.f",1:10)), .before = 'statistic')

) %>% 
  bind_rows(
    paste0("聲音", sprintf("%02.f",1:10)) %>% 
  lapply(function(x){
    
    DT.1 %>% 
      dplyr::select(姓名,Test, x) %>% 
      pivot_wider(names_from =Test, values_from = c(x))%>% 
      
      t.test(Pair(`前測`, `後測`) ~1, data = .)%>%
      .[c(1,3)] %>% bind_cols
    
  }) %>% bind_rows() %>% 
  mutate(tracect = paste0("聲音", sprintf("%02.f",1:10)), .before = 'statistic')
) %>% 
  bind_rows(
    
    c("score_方法","score_照片","score_聲音","score_綜合","總分") %>% 
  lapply(function(x){
    
    DT.1 %>% 
      dplyr::select(姓名,Test, x) %>% 
      pivot_wider(names_from =Test, values_from = c(x))%>% 
      
      t.test(Pair(`前測`, `後測`) ~1, data = .)%>%
      .[c(1,3)] %>% bind_cols
    
  })%>% bind_rows() %>% 
  mutate(tracect = paste0(c("score_方法","score_照片","score_聲音","score_綜合","總分")) ,
         .before = 'statistic')

)  %>% 
  setNames(., c("tracect", "t", "p.value"))%>% 
  mutate(p.value = round(p.value,2),
         t = round(-t,2)) %>% 
  mutate(tracect = str_replace_all(tracect, set_names(dt$題目$考題, dt$題目$題目)))

View(all_T.test,"all_T.test")
#----------------

library(openxlsx)

list(
  "DT" =DT.1 ,
  "all_T.test" = all_T.test
)%>%
  write.xlsx(.,"圖表_2025.xlsx")

