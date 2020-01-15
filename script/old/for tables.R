# for summary table

library(readxl)
library(data.table)
library(magrittr)
library(dplyr)
library(writexl)

Site.15 <- 
  fread("data/raw/2015樣區內獼猴調查含樣點數(20161109).csv") %>% 
  .[, list(County = `縣市`,
           Site = `樣點編號`)]
dat.15 <- 
  read_xlsx("data/raw/2015調查時段樣區內獼猴紀錄.xlsx",
            sheet = 3) %>% 
  setDT %>% 
  .[, list(Site = `樣區編號`,
           Point = as.character(`樣點編號`),
           Group = `結群`,
           Year = 2015)] %>% 
  Site.15[., on = "Site"]
dat.16 <- 
  fread("data/raw/2016樣區內獼猴調查(20161108).csv") %>% 
  .[地點 %in% "三峽竹崙", `樣區編號` := "A05-20"] %>% 
  .[, list(County = `縣市`,
           Site = `樣區編號`,
           Point = as.character(`樣點編號`),
           Group = `結群`,
           Year = 2016)]
Site.17 <- 
  read_xlsx("data/raw/2017獼猴調查(樣區整理)_分析用.xlsx",
            sheet = 2) %>% 
  setDT %>% 
  .[, list(County = `縣市`,
           Site = `樣區\r\n編號`)] %>% 
  unique
dat.17 <- 
  read_xlsx("data/raw/2017樣區內獼猴調查.xlsx") %>% 
  setDT %>% 
  .[, list(Site = `樣區編號`,
           Point = as.character(`樣點編號`),
           Group = `結群\r\n(修正)`,
           Year = 2017)] %>% 
  Site.17[., on = "Site"]
Site.18 <- 
  read_xlsx("data/raw/2018獼猴調查(樣區整理)_分析用.xlsx",
            sheet = 2) %>% 
  setDT %>% 
  .[, list(County = `縣市`,
           Site = `樣區\r\n編號`)] %>% 
  unique
dat.18 <- 
  read_xlsx("data/raw/2018樣區內獼猴調查.xlsx") %>% 
  setDT %>% 
  .[, list(Site = `樣區編號`,
           Point = as.character(`樣點編號`),
           Group = `結群`,
           Year = 2018)] %>% 
  Site.18[., on = "Site"]

Site.19 <- 
  read_xlsx("data/raw/2019獼猴調查(樣區整理)_分析用.xlsx",
            sheet = 2) %>% 
  setDT %>% 
  .[, list(County = `縣市`,
           Site = `樣區\r\n編號`)] %>% 
  unique
dat.19 <- 
  read_xlsx("data/raw/2019樣區內獼猴調查.xlsx") %>% 
  setDT %>% 
  .[, list(Site = `樣區編號`,
           Point = as.character(`樣點編號`),
           Group = `結群`,
           Year = 2019)] %>% 
  Site.19[., on = "Site"]

dat.all <- 
  bind_rows(dat.15, dat.16, dat.17, dat.18, dat.19) %>% 
  setDT %>%
  .[, Site.Point := paste(Site, Point, sep = "_")]

write_xlsx(dat.all,
           "data/clean/Macaca_survey_1519.xlsx")

#
sum.table <- 
  dat.all[, .(dataN = .N,
              SiteN = uniqueN(Site),
              PointN = uniqueN(Site.Point)),
          by = list(County, Year, Group)]
sum.table.Group <- 
  dcast(sum.table[Group == "Y"],
        County ~ Year, 
        value.var = c("dataN", "SiteN", "PointN"))
sum.table.Single <- 
  dcast(sum.table[Group == "N"],
        County ~ Year, 
        value.var = c("dataN", "SiteN", "PointN"))



#=======================================================================

M.data <- 
  read_xlsx("data/clean/data_for_analysis.xlsx") %>% 
  setDT %>% 
  .[, Year := as.numeric(Year)] %>% 
  .[, Year.re := Year - min(Year) + 1] %>%
  .[, high := substr(Site_N, 1, 1)] 
  



sum.TypeName.high.table<- M.data %>% setDT %>% 
  .[ Do.survey >0, list( Year,Survey,TypeName,  Macaca_sur,high)] %>%
  dcast(., Year + Survey + high ~ TypeName, fun = length)
group.TypeName.high.table<- M.data %>% setDT %>% 
  .[ Macaca_sur %in% c(1), list( Year,Survey,TypeName,  Macaca_sur,high)] %>%
  dcast(., Year + Survey + high ~ TypeName, fun = length)

sum.TypeName.table<- M.data %>% setDT %>% 
  .[ Do.survey >0, list( Year,Survey,TypeName,  Macaca_sur)] %>%
  dcast(., Year + Survey ~ TypeName, fun = length)
group.TypeName.table<- M.data %>% setDT %>% 
  .[ Macaca_sur %in% c(1), list( Year,Survey,TypeName,  Macaca_sur)] %>%
  dcast(., Year + Survey  ~ TypeName, fun = length)


sum.table<- M.data %>% setDT %>% 
  .[ Do.survey >0, list( Year,Survey,TypeName,  Macaca_sur)] %>%
  dcast(., Year ~ Survey , fun = length)
group.table<- M.data %>% setDT %>% 
  .[ Macaca_sur %in% c(1), list( Year,Survey,TypeName,  Macaca_sur)] %>%
  dcast(., Year ~ Survey , fun = length)

write_xlsx(list("Group" = sum.table.Group,
                "Single" = sum.table.Single,
                "TypeName.high" = sum.TypeName.high.table,
                "group.TypeName.high" = group.TypeName.high.table,
                "TypeName" = sum.TypeName.table,
                "group.TypeName" = group.TypeName.table),
           "Results/sum_table.xlsx")
