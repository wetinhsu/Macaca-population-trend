library(tidyverse)
library(readxl)
library(glmmTMB)
#-------------------------------------------------
# input filenames
dt_file <- "2023林業署訓練班測驗.xlsx"

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

# dt$後測 %>% split(., .$人名) %>% as.list() %>% 
#   map(function(x){ str_split(x, "、") %>% unlist 
#   })


# 
# dt %>% map(function(x){
#   x %>% 
#   split(., .$人名) %>% 
#     map(function(j) select(j,-`人名`))
#          })  %>%  View



# 對答案並篩選出完整測驗的資料

DT <- 
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
          .[32:34] %>% 
          str_split(., "、") %>% 
          map2(Answer[c(32:34)], ., function(x,y){ 
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
  separate(., col = "綜合3",
           into = paste0("綜合3", sep = "_", Answer$綜合3), sep = "、") %>%
  mutate_at(names(.)[33:ncol(.)], function(x){
    ifelse(x != 0, 1,0)
    })%>%
  mutate(score_方法 = 3*apply(select(.,starts_with("方法")),1,sum, na.rm = T)) %>%
  mutate(score_照片 = 3*apply(select(.,starts_with("照片")),1,sum, na.rm = T)) %>%
  mutate(score_聲音 = 3*apply(select(.,starts_with("聲音")),1,sum, na.rm = T)) %>%
  mutate(score_綜合 = 1*apply(select(.,starts_with("綜合")),1,sum, na.rm = T)) %>%
  mutate(總分 = score_方法 + score_照片 + score_聲音 + score_綜合)

View(DT,"DT")

 
DT.1 <- 
DT  %>%
  right_join(dt$`基本資料`,., by = c("姓名" = "人名")) %>% 
  filter(姓名 %in% all_day$人名 )%>% 
  mutate(身分 = str_replace_all(身分,c( ".*志工" = "志工",
                                   # "臨時人員" = "職員",
                                    "技術士|臨時人員|職員" = "森林護管員",
                                    
                                    "^阿里山生態.*" = "志工") )) %>% 
  mutate(調查 = case_when(
    是否執行過臺灣獼猴和繁殖鳥類調查 %in% '只有執行過獼猴調查' ~ "Monkey",
    是否執行過臺灣獼猴和繁殖鳥類調查 %in% '只有執行過繁殖鳥類調查' ~ "Bird",
    是否執行過臺灣獼猴和繁殖鳥類調查 %in% '臺灣獼猴和繁殖鳥類調查都有執行過' ~ "Both",
    是否執行過臺灣獼猴和繁殖鳥類調查 %in% '都未曾執行過' ~ "None"
  )) 

DT.1$是否執行過臺灣獼猴和繁殖鳥類調查 %>% table
DT.1$身分 %>% table

# 分析
model <- 
  glmmTMB(score_方法 ~  Test + 身分 + 調查 + (1|姓名), 
          data = DT.1, family = gaussian)

# 分析結果
analysis_summary <- 
  summary(model)$coefficients$cond %>% as.data.frame()

car::Anova(model)

anova(model)

DT.1 %>% 
 # filter(身分 %in% "志工") %>% 
 # filter(調查 %in% c("A", "B")) %>% 
  select(`Test`,`score_方法`:`score_綜合`) %>% 
  reshape2::melt(id = 1:2) %>% 
  group_by(Test,variable) %>% 
  
  ggplot(., aes(variable, value, Test))+
  geom_boxplot(aes(fill = Test))
  
  summarise(Count = mean(value)) %>% 
  ggplot(., aes(variable, Count, Test))+
  geom_bar(aes(fill= Test),stat='identity', position=position_dodge())


DT.1 %>% 
 # filter(身分 %in% "森林護管員") %>% 
  # filter(調查 %in% c("A", "B")) %>% 
  select(`Test`,`身分`,`score_方法`:`score_綜合`) %>% 
  reshape2::melt(id = 1:2) %>% 
  group_by(Test,`身分`,variable) %>% 
  summarise(分數 = sum(value)/length(value)) %>% 
  left_join(dt$`題目`, by = c("variable" ="題目")) %>%
  ggplot(., aes(身分, 分數, Test))+
  geom_bar(aes(fill= Test),stat='identity', position=position_dodge())+
  facet_wrap(vars(考題), ncol = 5)+
  labs(x = "身份", y = "答對比例")
  
DT.1 %>% 
  # filter(身分 %in% "森林護管員") %>% 
  # filter(調查 %in% c("A", "B")) %>% 
  select(`Test`,`調查`,`聲音1`:`聲音10`) %>% 
  reshape2::melt(id = 1:2) %>% 
  group_by(Test,`調查`,variable) %>% 
  summarise(分數 = sum(value)/length(value)) %>% 
  left_join(dt$`題目`, by = c("variable" ="題目")) %>%
  ggplot(., aes(調查, 分數, Test))+
  geom_bar(aes(fill= Test),stat='identity', position=position_dodge())+
  facet_wrap(vars(考題), ncol = 5)+
  labs(x = "調查", y = "答對比例")
