library(tidyverse)
library(readxl)
library(writexl)


site_list <- 
  read_xlsx("//10.40.1.138/Bird Research/BBSTW (20170612)/01_調查/分層隨機取樣的樣區清單 _20221031.xlsx",
            sheet = "樣區表") 


  library(DBI)
  con <- dbConnect(RSQLite::SQLite(),"D:/R/test/DB/P_BBS.db")
  list_Site <- dbReadTable(con, "list_Site")
  
  list_Site %>% 
    filter(樣區編號 %in% site_list$樣區編號) %>% 
    filter(!is.na(獼猴樣區編號)) %>% 
    write_xlsx(., "D:/待處理工作夾(做完要歸檔)/屬於分層隨機取樣樣區的林務局樣區.xlsx")
  