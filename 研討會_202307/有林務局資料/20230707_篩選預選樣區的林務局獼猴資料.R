
library(tidyverse)
library(readxl)
library(here)

Site_pre <- 
read_excel(
"./研討會_202307/有林務局資料/屬於分層隨機取樣樣區的林務局樣區.xlsx"
) %>% 
  select(樣區編號,獼猴樣區編號)



M.data <- 
  list.files(here("./data/clean/Forestry/for analysis/"),
             full.names = T,pattern = "xlsx$|xls$") %>% 
  lapply(., read_excel, sheet="Data", col_types = "text") %>% 
  bind_rows()


M.data %>% 
  right_join(., Site_pre, by = c("Site_N" = "獼猴樣區編號")) %>% 
  filter(!is.na(Macaca_sur)) %>% 
  filter(str_detect(analysis, "Y") ) %>% 
  write_xlsx(., "./研討會_202307/有林務局資料/data/OnlyForestydata_20-22.xlsx")

