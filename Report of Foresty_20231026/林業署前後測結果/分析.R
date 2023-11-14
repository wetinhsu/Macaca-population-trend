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



# 同時做前後測的人
all_day <- 
inner_join(dt$`前測`, dt$`後測`, by = "人名") %>% 
  select(人名)

# 統計前後測人數
person <- 
list(前測 = dt$`前測`, 後測 =  dt$`後測`, Both = all_day) %>% 
  map(., nrow) %>% bind_rows(.id = "A")


# 對答案並篩選出完整測驗的資料
DT <-
dt[1:2] %>% 
  map(., function(x){
    
    mapply(match, x[,2:31], dt$解答[,2:31], SIMPLIFY = T) %>% 

      as.data.frame() %>% 
    
      mutate( as.data.frame(x[,1]), .before = "方法1")
    #答對的為1
  }) %>% 
  bind_rows(.id = "Test") %>% 
  right_join(dt$`基本資料`,., by = c("姓名" = "人名")) %>% 
  mutate(score = 3*apply(.[,5:34],1,sum, na.rm = T)) %>% 
  filter(姓名 %in% all_day$人名 )



function(x,y){  ifelse( x %in% y, 1,0)}















# 沒答對的為0
DT[is.na(DT)] <-  0  


# 分析
model <- 
glmmTMB(score ~  Test + 年 + 性別 + (1|姓名), 
        data = DT, family = gaussian)

# 分析結果
analysis_summary <- 
summary(model)$coefficients$cond %>% as.data.frame()

# 簡單畫圖，不儲存圖
DT %>% 
  select(`Test`:`聲音10`) %>% 
  reshape2::melt(id = 1) %>% 
  group_by(Test,variable) %>% 
  summarise(Count = sum(value)) %>% 
  ggplot(., aes(variable, Count, Test))+
  geom_bar(aes(fill= Test),stat='identity', position=position_dodge())


#-------------------------------
library(openxlsx2)
library(mschart)
# https://cran.r-project.org/web/packages/openxlsx2/vignettes/openxlsx2_charts_manual.html

#整理data成可以畫成excel圖的格式
dat_split <-
DT %>% 
  select(`Test`:`聲音10`) %>% 
  reshape2::melt(id = 1) %>% 
  group_by(Test,variable) %>% 
  summarise(Count = sum(value)) %>% 
  left_join(dt$`題目`, by = c("variable" ="題目")) %>% 
  mutate(大項 = str_extract(variable,"\\w{1,2}")) %>% 
  split(., .$大項) %>% 
  map(., function(x){
    x %>% 
      reshape2::dcast(variable+ 考題 ~Test, value.var= "Count") %>% 
      mutate(NO = str_extract(variable,"\\d{1,}")) %>% 
      arrange(as.numeric(NO)) %>% 
      select(-NO)
  })

# 將資料寫進EXCEL裡
wb <- wb_workbook() %>%
  wb_add_worksheet() %>% 
  wb_add_data(x = dat_split$方法, dims = "A1")%>% 
  wb_add_data(x = dat_split$照片, dims = "A13")%>% 
  wb_add_data(x = dat_split$聲音, dims = "A25")

#設定圖的版面格式
mytheme <- mschart_theme(
  axis_title_x = fp_text(color = "gray", font.size = 20, bold = TRUE),
  axis_title_y = fp_text(color = "gray", font.size = 20, italic = TRUE),
  grid_major_line = fp_border(width = 0),
  grid_minor_line = fp_border(width = 0),
  axis_ticks_y = fp_border(width = 1, color = "gray"),
              legend_position = "t"
)

# 畫barchart #方法
barchart_plot_1 <- 
  wb_data(wb, dims = "A1:D11") %>% 
  ms_barchart(
    data = .,
    x = "考題",
    y = c("前測","後測"),
    label = c("前測","後測")
  )%>% 
  chart_labels(., title = "方法測驗") %>% 
  chart_data_labels(., position = "outEnd") %>% 
  chart_labels_text(., fp_text(font.size = 11)) %>% 
  set_theme(., mytheme) 

# 畫barchart #照片
barchart_plot_2 <- 
  wb_data(wb, dims = "A13:D23") %>% 
  ms_barchart(
    data = .,
    x = "考題",
    y = c("前測","後測"),
    label = c("前測","後測")
  )%>% 
  chart_labels(., title = "照片測驗") %>% 
  chart_data_labels(., position = "outEnd") %>% 
  chart_labels_text(., fp_text(font.size = 11)) %>% 
  set_theme(., mytheme) 

# 畫barchart #鳥音
barchart_plot_3 <- 
  wb_data(wb, dims = "A25:D35") %>% 
  ms_barchart(
    data = .,
    x = "考題",
    y = c("前測","後測"),
    label = c("前測","後測")
)%>% 
  chart_labels(., title = "鳥音測驗") %>% 
  chart_data_labels(., position = "outEnd")%>% 
  chart_labels_text(., fp_text(font.size = 11)) %>% 
  set_theme(., mytheme) 

# 將3個barchart圖及分析結果寫進excel
wb <- wb %>%
  wb_add_mschart(., dims = "F4:S20", graph = barchart_plot_1) %>%
  wb_add_mschart(., dims = "F21:S37", graph = barchart_plot_2) %>%
  wb_add_mschart(., dims = "F38:S54", graph = barchart_plot_3) %>%
  wb_add_data(x = person , dims = "J1" ) %>% 
  wb_add_worksheet(sheet = "analysis")%>% 
  wb_add_data(x = "glmmTMB(score ~  Test + 年 + 性別 + (1|姓名), data = DT, family = gaussian)",
              sheet = "analysis", dims = "A3",
              rowNames = F)%>% 
  wb_add_data(x = analysis_summary, sheet = "analysis", dims = "A6",
              rowNames = T)

#預覽excel
xl_open(wb)

# output
wb_save(wb, path = "分析結果.xlsx", overwrite = TRUE)
#---------------------------------------------------

#以下 用glmm檢測各題前後測是否有顯著差異，先看看沒有用在報告上，報告是看圖而已。

glmmTMB(方法1 ~  Test + 年 + 性別 + (1|姓名), 
        data = DT, family = binomial())%>% summary()


#分析全部的題目

DT %>% reshape2::melt(1:4) %>% 
  split(., .$variable) %>% 
  map(., function(x){
    tryCatch(
      expr = {
    glmmTMB(value ~  Test + 年 + 性別 + (1|姓名), 
            data = x, family = binomial())%>% summary()
      },
    
    error = function(e){ print("error") } )
    
  })
