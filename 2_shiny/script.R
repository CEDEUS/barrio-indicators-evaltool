library(tidyverse)

dataset <- read_csv("data.csv")

# Unit counter

unit <- dataset %>% group_by(d,manzb,codename) %>% summarise(n=n()) %>% spread(manzb,n)
