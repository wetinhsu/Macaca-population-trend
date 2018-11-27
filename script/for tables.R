# for summary table

library(readxl)
library(data.table)
library(magrittr)
library(dplyr)

dat.15 <- 
  read_xlsx("data/raw/2015調查時段樣區內獼猴紀錄.xlsx",
            sheet = 3) %>% 
  setDT %>% 
  .[, list(Site = `樣區編號`,
           Point = `樣點編號`,
           Group = `結群`)] %>% 
  Site.17[., on = "Site"]
dat.16 <- 
  fread("data/raw/2016樣區內獼猴調查(20161108).csv") %>% 
  .[, list(County = `縣市`,
           Site = `樣區編號`,
           Point = `樣點編號`,
           Group = `結群`)]
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
           Point = `樣點編號`,
           Group = `結群\r\n(修正)`)] %>% 
  Site.17[., on = "Site"]
Site.18 <- 
  read_xlsx("data/raw/2018獼猴調查(樣區整理)_分析用.xlsx",
            sheet = 2) %>% 
  setDT %>% 
  .[, list(County = `縣市`,
           Site = `樣區\r\n編號`)]
dat.18 <- 
  read_xlsx("data/raw/2018樣區內獼猴調查.xlsx") %>% 
  setDT %>% 
  .[, list(Site = `樣區編號`,
           Point = `樣點編號`,
           Group = `結群`)] %>% 
  Site.18[., on = "Site"]

dat.all <- 
  bind_rows(dat.15, dat.16, dat.17, dat.18)