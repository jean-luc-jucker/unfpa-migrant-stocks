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

?geom_area











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



#switzerland####

#barplot of switzerland
data %>% 
  mutate(Stock=Stock/1000000) %>% 
  filter(Area=="Switzerland") %>% 
  ggplot(aes(x=Area, y=Stock, fill=as.factor(Year)))+
  geom_col(position = "dodge")

#line plot of switzerland
data %>% 
  mutate(Stock=Stock/1000000) %>% 
  filter(Area=="Switzerland") %>% 
  ggplot(aes(x=Year, y=Stock, color=Area))+
  geom_point()+
  geom_line(group=1)+
  scale_y_continuous(breaks = seq(0, 3, by = 0.1))+
  scale_x_continuous(breaks = seq(0, 2020, by = 5))+
  theme_bw()+
  ylab("Migrant Stock (millions)")+
  theme(axis.title.x = element_text(vjust=-1))+
  theme(axis.title.y = element_text(vjust=2.5))+
  theme(legend.title = element_blank())






?stat_smoothp2 <- data_long %>%
  filter(!is.na(Victim)) %>% 
  count(Gender, Incident, Victim) %>% 
  group_by(Gender, Incident) %>% 
  mutate(Percent = round(n/sum(n)*100)) %>%  
  filter(Victim=="Yes") %>% 
  ungroup() %>% #note it is crucial to ungroup here to get the sorting right!
  mutate(Incident = reorder(Incident, -Percent)) %>% #sorting
  ggplot(aes(x=Incident, y=Percent, fill=Gender))+
  geom_col(position = "dodge", width=0.72) +
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  theme_bw()+
  labs(fill=NULL)+
  theme(axis.text.x = element_text(size=16), axis.text.y = element_text(size=16))+
  theme(axis.title = element_text(size = 18))+
  theme(legend.text = element_text(size = 16))+
  ylab("Percent of Participants")+
  theme(legend.position = c(0.88, 0.95), legend.direction = "horizontal")+
  scale_x_discrete(labels= incidents_labels)+
  theme(axis.title.x = element_text(vjust=-1))+
  theme(axis.title.y = element_text(vjust=2.5))

##################

tinytex::install_tinytex


update.packages(ask = FALSE, checkBuilt = TRUE)  # update R packages
tinytex::tlmgr_update()  # update LaTeX packages
install.packages("tinytex")
install.packages("latexpdf")
library(tinytex)

