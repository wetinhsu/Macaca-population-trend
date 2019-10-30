
#---- load library

library(data.table)
library(magrittr)
library(readxl)
library(tidyr)
library(purrr)
library(dplyr)
library(writexl)
library(rgdal)


#====================



bbb <-  list.files()%>% 
   lapply(., function(x) {
      read_excel(x, sheet = 1, cell_cols("A:U"),col_names = T)
    }) %>%
  do.call(rbind, .)  %>% setDT

summary( bbb)

write_xlsx(bbb, "bind.xlsx")
 