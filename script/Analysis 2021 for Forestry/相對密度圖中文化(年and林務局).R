library(showtext)

showtext_auto()
font_add("Microsoft JhengHei", "msjh.ttc")

M.data %>% 
  group_by(Year, Survey) %>% 
  summarise(
    N = n(),
    m = Macaca_sur %>% as.numeric() %>% sum,
    E = m/N
  ) %>% 
  ggplot(., aes(x = factor(Year), y = E)) +
  geom_boxplot()+
  scale_y_continuous(limits =  c(0, 0.1),
                     breaks = seq(0,0.1,0.02))+
  labs(x = "年",
       y = "相對密度 (群/ 樣點)")+
  theme_classic()+
  theme(
    panel.border = element_blank(),
    text = element_text(size = 30,
                        family = "Microsoft JhengHei"),
    axis.title.y = element_text(angle = 270, face = "bold", vjust = 0),
    axis.text = element_text(colour = "black")
  )

ggsave("Year.png",
       path ="./林務局年報2020-2021/")

#--------------


M.data %>% 
  group_by(Office, Year,Survey) %>% 
  summarise(
    N = n(),
    m = Macaca_sur %>% as.numeric() %>% sum,
    E = m/N
  ) %>% 
  mutate(Office = 
           ordered(Office,
                   labels = c( "新竹", "東勢", "南投","羅東",
                              "嘉義", "屏東", "花蓮", "臺東"),
                   levels = c("Hsinchu", "Dougshih", "Nantou","Luodong", 
                              "Chiayi", "Pingtung", "Hualien", "Taitung")
           )
  ) %>% 
  
  ggplot(., aes(x = factor(Office), y = E)) +
  geom_boxplot()+
  scale_y_continuous(limits =  c(0, 0.12),
                     breaks = seq(0,0.12,0.02))+
  labs(x = "林管處",
       y = "相對密度 (群/ 樣點)")+
  theme_classic()+
  theme(
    panel.border = element_blank(),
    text = element_text(size = 30,
                        family = "Microsoft JhengHei"),
    axis.title.y = element_text(angle = 270, face = "bold", vjust = 0),
    axis.text = element_text(colour = "black")
  )


ggsave("Office.png",
       path ="./林務局年報2020-2021/")
