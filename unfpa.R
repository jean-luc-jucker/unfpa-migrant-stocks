library(tidyverse)
library(readxl)
library(plyr)
getwd()

data <- read_excel("data/UN_MigrantStockTotal_2019.xlsx", sheet = "Table 1", skip = 14)

data <- data %>% 
  select(-c(1, 3, 5, 13:26)) %>% 
  rename(replace = c("Major area, region, country or area of destination" = "Area", "International migrant stock at mid-year (both sexes)" = "1990", "...7"="1995", "...8"="2000", "...9"="2005", "...10"="2010", "...11"="2015", "...12"="2019")) %>% 
  filter(!is.na(Area)) %>% 
  pivot_longer(cols = 3:9 , names_to = "Year", values_to = "Stock") %>% 
  filter(Stock != "..")

#convert to factor/numeric
data[c(1:2)] <- lapply(data[c(1:2)], as.factor)
data[c(3:4)] <- lapply(data[c(3:4)], as.numeric)

save(data, file = "rda/unfpa.rda") 
