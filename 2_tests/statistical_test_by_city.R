library(tidyverse)

# Databases
barrios_m <- readRDS("Data/barrios_merged.RDS")
manzbarr <- readRDS("Data/Manzanasbarriospiloto.RDS")
dataset <- read_csv("/Users/robsalasco/Dev/barrio-indicators-evaltool/data.csv")

dataset %>% 
  slice_rows(c("CIUDAD","codename")) %>% 
  by_slice(~tidy(shapiro.test(.x$listo)), .collate = "rows") %>% 
  mutate(normal=ifelse(p.value>0.05,1,0))

dataset %>% 
  slice_rows(c("CIUDAD","codename")) %>% 
  by_slice(~tidy(with(.x, wilcox.test(as.numeric(listo),as.numeric(manzb)))), .collate = "rows")
