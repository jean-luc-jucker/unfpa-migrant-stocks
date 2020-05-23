library(tidyverse)
getwd()
load("rda/unfpa_percent.rda")
dim(data)
str(data)
summary(data)
View(data)

####Visualisations########

#world####

#region#####UPDATED
data %>% 
  filter(Area=="Africa" | Area=="Asia"| Area=="Europe"| Area=="Latin America and the Caribbean"| Area=="Northern America"| Area=="Oceania") %>% 
  mutate(Area=recode(Area,'Latin America and the Caribbean'="Latin America")) %>% 
  mutate(Area=reorder(Area, -Percent)) %>% 
  ggplot(aes(x=Year, y=Percent, color=Area))+ 
  geom_point()+
  geom_line()+
  scale_y_continuous(breaks = seq(0, 30, by = 1))+
  scale_x_continuous(breaks = seq(0, 2020, by = 5))+
  theme_bw()+
  ylab("Percent of population")+
  theme(axis.title.x = element_text(vjust=-1))+
  theme(axis.title.y = element_text(vjust=2.5))+
  theme(legend.title = element_blank())
  
ggsave("figs/by_area.png") 

#same, stacked area
data %>% 
  mutate(Stock=Stock/1000000) %>% 
  filter(Area=="Africa" | Area=="Asia"| Area=="Europe"| Area=="Latin America and the Caribbean"| Area=="Northern America"| Area=="Oceania") %>% 
  mutate(Area=recode(Area,'Latin America and the Caribbean'="Latin America")) %>% 
  mutate(Area=reorder(Area, -Stock)) %>% 
  ggplot(aes(x=Year, y=Stock, fill=Area))+ 
  geom_area() +
  scale_y_continuous(breaks = seq(0, 300, by = 20))+
  scale_x_continuous(breaks = seq(0, 2020, by = 5))+
  theme_bw()+
  ylab("Migrant Stock (millions)")+
  theme(axis.title.x = element_text(vjust=-1))+
  theme(axis.title.y = element_text(vjust=2.5))+
  theme(legend.title = element_blank())
ggsave("figs/by_area_stacked.png") 

#same, stacked proportional
data %>% 
  mutate(Stock=Stock/1000000) %>% 
  filter(Area=="Africa" | Area=="Asia"| Area=="Europe"| Area=="Latin America and the Caribbean"| Area=="Northern America"| Area=="Oceania") %>% 
  mutate(Area=recode(Area,'Latin America and the Caribbean'="Latin America")) %>% 
  mutate(Area=reorder(Area, -Stock)) %>% 
  ggplot(aes(x=Year, y=Stock, fill=Area))+ 
  geom_area(position = "fill") +
  scale_x_continuous(breaks = seq(0, 2020, by = 5))+
  theme_bw()+
  ylab("Migrant Stock (millions)")+
  theme(axis.title.x = element_text(vjust=-1))+
  theme(axis.title.y = element_text(vjust=2.5))+
  theme(legend.title = element_blank())
ggsave("figs/by_area_stacked.png") 

#european countries (selection of 16)####UPDATED
data %>% 
  filter(Area=="Finland" | Area=="Iceland"| Area=="Ireland"| Area=="Norway"| Area=="Sweden"| Area=="United Kingdom"| Area=="Greece"| Area=="Italy"| Area=="Portugal"| Area=="Spain"| Area=="Austria"| Area=="Belgium"| Area=="France"| Area=="Germany"| Area=="Netherlands"| Area=="Switzerland") %>% 
  mutate(Area=reorder(Area, -Percent)) %>% 
  ggplot(aes(x=Year, y=Percent, color=Area))+ 
  geom_point()+
  geom_line()+
  scale_y_continuous(breaks = seq(0, 40, by = 1))+
  scale_x_continuous(breaks = seq(0, 2020, by = 5))+
  theme_bw()+
  ylab("Percent of Population")+
  theme(axis.title.x = element_text(vjust=-1))+
  theme(axis.title.y = element_text(vjust=2.5))+
  theme(legend.title = element_blank())

#european countries (border countries)####
data %>% 
  filter(Area=="Italy"| Area=="Austria"| Area=="France"| Area=="Germany"| Area=="Switzerland") %>% 
  mutate(Area=reorder(Area, -Percent)) %>% 
  ggplot(aes(x=Year, y=Percent, color=Area))+ 
  geom_point()+
  geom_line()+
  scale_y_continuous(breaks = seq(0, 40, by = 1))+
  scale_x_continuous(breaks = seq(0, 2020, by = 5))+
  theme_bw()+
  ylab("Percent of Population")+
  theme(axis.title.x = element_text(vjust=-1))+
  theme(axis.title.y = element_text(vjust=2.5))+
  theme(legend.title = element_blank())

#same, stacked area
data %>% 
  mutate(Stock=Stock/1000000) %>% 
  filter(Area=="Finland" | Area=="Iceland"| Area=="Ireland"| Area=="Norway"| Area=="Sweden"| Area=="United Kingdom"| Area=="Greece"| Area=="Italy"| Area=="Portugal"| Area=="Spain"| Area=="Austria"| Area=="Belgium"| Area=="France"| Area=="Germany"| Area=="Netherlands"| Area=="Switzerland") %>% 
  mutate(Area=reorder(Area, -Stock)) %>% 
  ggplot(aes(x=Year, y=Stock, fill=Area))+ 
  geom_area()+
  scale_y_continuous(breaks = seq(0, 80, by = 5))+
  scale_x_continuous(breaks = seq(0, 2020, by = 5))+
  theme_bw()+
  ylab("Migrant Stock (millions)")+
  theme(axis.title.x = element_text(vjust=-1))+
  theme(axis.title.y = element_text(vjust=2.5))+
  theme(legend.title = element_blank())

#same, stacked proportional
data %>% 
  mutate(Stock=Stock/1000000) %>% 
  filter(Area=="Finland" | Area=="Iceland"| Area=="Ireland"| Area=="Norway"| Area=="Sweden"| Area=="United Kingdom"| Area=="Greece"| Area=="Italy"| Area=="Portugal"| Area=="Spain"| Area=="Austria"| Area=="Belgium"| Area=="France"| Area=="Germany"| Area=="Netherlands"| Area=="Switzerland") %>% 
  mutate(Area=reorder(Area, -Stock)) %>% 
  ggplot(aes(x=Year, y=Stock, fill=Area))+ 
  geom_area(position = "fill")+
  scale_x_continuous(breaks = seq(0, 2020, by = 5))+
  theme_bw()+
  ylab("Migrant Stock (millions)")+
  theme(axis.title.x = element_text(vjust=-1))+
  theme(axis.title.y = element_text(vjust=2.5))+
  theme(legend.title = element_blank())


#africa
africa <- data %>% 
  slice(c(134:269, 277:339, 347:381, 389:507, 522:570)) %>% 
  mutate(Area = recode(Area, 'Democratic Republic of the Congo'='Congo', 'United Republic of Tanzania'='Tanzania'))
africa %>% 
  mutate(Stock=Stock/1000000) %>% 
  filter(Stock > 0.9) %>% 
  mutate(Area=reorder(Area, Stock)) %>% 
  ggplot(aes(as.factor(Year), Area, fill=Stock))+
  geom_tile()+ 
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(3, "Greens"))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  ylab("Country")+xlab("Year")+
  theme(axis.title.x = element_text(vjust=-1))+
  labs(fill = "Stock (millions)")



#asia
asia <- data %>% 
  slice(c(718:752, 760:822, 837:885, 893:969))%>% 
  mutate(Area = recode(Area, 'Iran (Islamic Republic of)'='Iran', 'China, Hong Kong SAR'='Hong Kong'))
asia %>% 
  mutate(Stock=Stock/1000000) %>% 
  filter(Stock > 1.8) %>% 
  mutate(Area=reorder(Area, Stock)) %>% 
  ggplot(aes(as.factor(Year), Area, fill=Stock))+
  geom_tile()+ 
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(3, "Greens"))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  ylab("Country")+xlab("Year")+
  theme(axis.title.x = element_text(vjust=-1))+
  labs(fill = "Stock (millions)")



#europe UPDATED
europe <- data %>% 
  slice(c(1543:1612, 1620:1710, 1718:1825, 1833:1895)) %>% 
  mutate(Area = recode(Area, 'United Kingdom'='UK'))

europe %>% 
  filter(Area!="Russian Federation") %>% 
  filter(Percent > 10) %>% 
  mutate(Area=reorder(Area, Percent)) %>% 
  ggplot(aes(as.factor(Year), Area, fill=Percent))+
  geom_tile()+ 
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(3, "Greens"))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  ylab("Country")+xlab("Year")+
  theme(axis.title.x = element_text(vjust=-1))+
  labs(fill = "Stock (millions)")

#world--2019
world <- data %>% 
  filter(Year=="2019") %>% 
  slice(c(-1:-19, -40, -50, -56, -74, -75, -83, -102, -103, -109, -119, -120, -128, -140, -141, -168, -177, -192, -193, -196, -202, -210, -220:-222, -233, -247, -264, -274)) %>% 
  mutate(Area = recode(Area, 'Democratic Republic of the Congo'='Congo', 'United Republic of Tanzania'='Tanzania', 'United States of America'='USA', 'China, Hong Kong SAR' = 'Hong Kong', 'Iran (Islamic Republic of)'='Iran', 'Venezuela (Bolivarian Republic of)'='Venezuela', 'Republic of Korea'='South Korea', 'United Arab Emirates'='Emirates', 'Russian Federation'='Russia', 'United Kingdom' = 'UK'))

#version 1 UPDATED
world %>% 
  filter(Percent>25) %>% 
  mutate(Area=reorder(Area, Percent)) %>% 
  ggplot(aes(x=Area, y=Percent, fill = Area))+
  geom_col()+
  theme_bw()+
  theme(legend.position = "none")+
  scale_y_continuous(breaks = seq(0, 100, by = 3))+
  ylab("Percent")+xlab("Country")+
  theme(axis.title.y = element_text(vjust=2))+
  coord_flip()
  
  

#version 2 UPDATED
world %>% 
  filter(Percent>25) %>% 
  mutate(Area=reorder(Area, Percent)) %>% 
  ggplot(aes(x=Area, y=Percent, fill = Area))+
  geom_col()+
  theme_bw()+
  theme(legend.position = "none")+
  scale_y_continuous(breaks = seq(0, 60, by = 2))+
  coord_flip()+
  ylab("Stock (millions)")+xlab("Country")+
  theme(axis.title.x = element_text(vjust=-1))

#world, stacked proportiona

world2 <- data %>% 
  filter(as.numeric(as.character(Code))<=894) %>% 
  mutate(Stock=Stock/1000000)

world2 %>%
  filter(Stock>7) %>% 
  mutate(Area=reorder(Area, Stock)) %>% 
  ggplot(aes(x=Year, y=Stock, fill=Area))+
  geom_area(position = "fill")


?geom_area
