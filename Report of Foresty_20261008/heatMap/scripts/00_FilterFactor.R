library(ggcorrplot)
library(tidyverse)
library(sf)
library(fs)



library(terra)
sf_path <- "D:/R/SHP圖層/Taiwan_environmental_dataset-master/GeoTIFF"


envo_ls <- 
  dir_ls(sf_path, recurse = T, type = "file") %>% 
  .[str_detect(., "TIF")] %>% 
  .[str_detect(., "121")] %>% 
  .[str_detect(., "2010s|Other|DTM") ] %>% 
  .[str_detect(., "zip", negate = T) ] 


environment_sp <- rast(envo_ls) 
#--
environment_sp <- 
  data.frame(environment_sp) %>%
  setNames(str_remove_all(colnames(.), "G1km_TWD97.121_|_2010s"))


cor_matrix <-
  data.frame(environment_sp) %>% 
  dplyr::select(matches("Other")) %>% 
  dplyr::select(-matches("DSW")) %>% 
  cor(., method = c("pearson"), use = "pairwise.complete.obs") 

 # ggcorrplot(cor_matrix, 
 #            method = "square", 
 #            type = "lower",        # 顯示下三角矩陣
 #            lab = TRUE,            # 顯示相關係數數值
 #            lab_size = 3,          # 數字大小
 #            colors = c("red", "white", "blue"), # 負相關為紅，正相關為藍
 #            title = "變數相關係數熱力圖",
 #            ggtheme = ggplot2::theme_minimal())
cor_matrix

cor_df <- as.data.frame(as.table(cor_matrix)) %>%
  rename(Var1 = Var1, Var2 = Var2, Cor = Freq)

filtered_cor_abs <- cor_df %>%
  filter(as.character(Var1) < as.character(Var2)) %>% 
  filter(abs(Cor) > 0.01)

head(filtered_cor_abs)


#===以下不用============================

# 安裝與載入套件（若未安裝執行 install.packages("ClustOfVar")）
library(ClustOfVar)

# 1. 假設 sub_data 是你某個類別（例如降水類）的原始資料框
# 執行階層變數集群分析
tree <- data.frame(environment_sp) %>% 
  select(matches( 'DTM')) %>%
  hclustvar(.)

# 2. 直接切成 1 群（或指定你要的群數），尋找該群的代表變數
# cutreevar 會自動選出該群中綜合表現最強、最具代表性的變數 (Medoid)
partition <- cutreevar(tree, k = 1)

# 3. 查看該群的代表性變數名稱
print(partition$var$name)


#----------
# 相關矩陣平均相關度最高法（找出該類別的「中心點 / Medoid」）
# 計算該類別內每個變數與其他所有變數的平均相關係數。平均相關係數最高的那個變數，代表它與群體內其他成員的資訊重疊度最高，最能代表整個群體的共同趨勢。




# 假設 temp_df 是你該類別（如溫度類）的所有變數欄位
# 1. 計算相關矩陣
cor_mat <- data.frame(environment_sp) %>% 
  select(matches( 'Climate')) %>% 
  cor(., use = "pairwise.complete.obs")

# 2. 計算每個變數與其他所有變數的「平均絕對相關係數」
mean_cor <- colMeans(abs(cor_mat))

# 3. 排序並印出結果，最高分的就是最佳代表項目
print(sort(mean_cor, decreasing = TRUE))

# 4. 自動抓出代表名稱
best_var <- names(which.max(mean_cor))
cat("該類別最能代表其他項目的變數是：", best_var, "\n")


#-----------------

