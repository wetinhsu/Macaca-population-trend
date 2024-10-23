


Surveyer.1517 <- 
  lapply(paste0("./data/raw/BBSdata/", 2015:2017), function(x){
    list.files(x, pattern = "BBSdata_", full.names = T) %>%  
      read_xlsx(., sheet = "birddata", cell_cols("D:AF")) %>% 
      setDT %>%
      #.[時段 %in% c("0-3minutes", "3-6minutes"),] %>%
      .[!(時段 %in% "Supplementary"),] %>%
      .[調查旅次編號 %in% c(1,2)] %>%
      .[ !分析 %in% "N", list(年, 樣區編號, 樣點編號, 調查旅次編號, 調查者)] %>%
      setnames(., c("Year", "Site_N", "Point",  "Survey", "Surveyer")) %>%
      .[!duplicated(.)]  
  } ) %>% 
  do.call(rbind, .) %>%
  
  separate(.,Surveyer,
           into = c("Surveyer_0","Surveyer_1","Surveyer_2","Surveyer_3"),
           sep ="、", extra = "drop", fill = "right") %>% 
  reshape2::melt(.,id.vars = c("Year", "Site_N", "Point",  "Survey"),
                 variable.name = "Surveyer", value.name = "Name",) %>% 
  filter(!is.na(Name)) %>% 
  
  unique(.) 


Surveyer.1821 <- 
  lapply(paste0("./data/raw/BBSdata/", 2018:2021), function(x){
    list.files(x, pattern = "BBSdata_", full.names = T) %>%  
      read_xlsx(., sheet = "birddata", cell_cols("B:AF"),col_types ="text") %>% 
      setDT %>%
      #.[時段 %in% c("0-3minutes", "3-6minutes"),] %>%
      .[!(時段 %in% "Supplementary"),] %>%
      .[調查旅次編號 %in% c(1,2)] %>%
      .[ !分析 %in% "N", list(年, 樣區編號, 樣點編號, 調查旅次編號, 調查者)] %>%
      setnames(., c("Year", "Site_N", "Point",  "Survey", "Surveyer")) %>%
      .[!duplicated(.)]  
  } ) %>% 
  do.call(rbind, .)%>%
  
  separate(.,Surveyer,
           into = paste0("Surveyer","_",0:10),
           sep ="、", extra = "drop", fill = "right") %>% 
  reshape2::melt(.,id.vars = c("Year", "Site_N", "Point",  "Survey"),
                 variable.name = "Surveyer", value.name = "Name",) %>% 
  filter(!is.na(Name)) %>% 
  
  unique(.) 


Name.list <- 
  rbind(Surveyer.1517,
        Surveyer.1821) %>% 
  unique(.) %>% 
  
  mutate(Name = gsub("*.(老鳥.*).*|*.(幕後.*).*", "",Name)) %>% 
  mutate(Name = gsub("Tefon", "吳杰峰",Name)) %>%
  mutate(Name = gsub("呂效修", "呂効修",Name)) %>%
  mutate(Name = gsub("劉晉笠", "劉晉岦",Name)) %>%
  mutate(Name = gsub("黃淳禛", "黃淳禎",Name)) %>%
  mutate(Name = gsub("簡美棋", "簡美祺",Name)) %>%
  mutate(Name = gsub("魏硯昀", "魏硯畇",Name)) %>%
  filter(! Name %in% "大安社大學員6人")


Name.list %>% 
  group_by(Year) %>% 
  summarise(
    n_site = Site_N %>% unique() %>% length,
    n_person = Name %>% unique() %>% length
  ) %>% 
  reshape2::melt(id = 1) %>% 
  ggplot(., aes(Year,value, group = variable))+
  geom_point(aes(col = variable))+
  geom_line(aes(col = variable))
  

