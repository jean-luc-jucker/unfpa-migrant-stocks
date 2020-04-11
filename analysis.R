library(tidyverse)
getwd()
load("rda/unfpa.rda")
dim(data)
str(data)
summary(data)

####Visualisations########

#world####

#region#####
data %>% 
  mutate(Stock=Stock/1000000) %>% 
  filter(Area=="Africa" | Area=="Asia"| Area=="Europe"| Area=="Latin America and the Caribbean"| Area=="Northern America"| Area=="Oceania") %>% 
  mutate(Area=recode(Area,'Latin America and the Caribbean'="Latin America")) %>% 
  mutate(Area=reorder(Area, -Stock)) %>% 
  ggplot(aes(x=Year, y=Stock, color=Area))+ 
  geom_point()+
  geom_line()+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  scale_x_continuous(breaks = seq(0, 2020, by = 5))+
  theme_bw()+
  ylab("Migrant Stock (millions)")+
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

#european countries (selection of 16)####
data %>% 
  mutate(Stock=Stock/1000000) %>% 
  filter(Area=="Finland" | Area=="Iceland"| Area=="Ireland"| Area=="Norway"| Area=="Sweden"| Area=="United Kingdom"| Area=="Greece"| Area=="Italy"| Area=="Portugal"| Area=="Spain"| Area=="Austria"| Area=="Belgium"| Area=="France"| Area=="Germany"| Area=="Netherlands"| Area=="Switzerland") %>% 
  mutate(Area=reorder(Area, -Stock)) %>% 
  ggplot(aes(x=Year, y=Stock, color=Area))+ 
  geom_point()+
  geom_line()+
  scale_y_continuous(breaks = seq(0, 20, by = 1))+
  scale_x_continuous(breaks = seq(0, 2020, by = 5))+
  theme_bw()+
  ylab("Migrant Stock (millions)")+
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

#smaller selection (continental Europe)
data %>% 
  mutate(Stock=Stock/1000000) %>% 
  filter(Area=="Italy"| Area=="Portugal"| Area=="Spain"| Area=="Austria"| Area=="Belgium"| Area=="France"| Area=="Netherlands"| Area=="Switzerland") %>% 
  ggplot(aes(x=Year, y=Stock, color=Area))+ 
  geom_point()+
  geom_line()

#last version

