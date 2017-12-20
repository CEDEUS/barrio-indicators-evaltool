library(tidyverse)

# Database
dataset <- read_csv("/Users/robsalasco/Dev/barrio-indicators-evaltool/data.csv")

dataset %>% 
  slice_rows(c("CIUDAD","codename")) %>% 
  by_slice(~tidy(shapiro.test(.x$listo)), .collate = "rows") %>% 
  mutate(normal=ifelse(p.value>0.05,1,0))

dataset %>% 
  slice_rows(c("CIUDAD","codename")) %>% 
  by_slice(~tidy(with(.x, wilcox.test(as.numeric(listo),as.numeric(manzb)))), .collate = "rows")

stage_3_nonormal_test <- dataset %>% 
  slice_rows(c("codename")) %>% 
  by_slice(~tidy(kruskal.test(.x$listo~factor(.x$LOCATION)))) %>%
  unnest() %>% select(codename,p.value,method)

for(x in vars){
  print(x)
  dunn.test(subset(dataset,codename==x)$listo,factor(subset(dataset,codename==x)$LOCATION),list=T,table = F,kw=F)
}
