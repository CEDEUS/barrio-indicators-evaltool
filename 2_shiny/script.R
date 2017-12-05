library(tidyverse)

barrios_m <- readRDS("Data/barrios_merged.RDS")
manzbarr <- readRDS("Data/Manzanasbarriospiloto.RDS")

# Unit counter

ub <- barrios_m@data %>% group_by(BARRIO) %>% summarise(n=n())
uba <- manzbarr %>% group_by(BARRIO) %>% summarise(n=n())

ubj <- right_join(ub,uba,by="BARRIO")
rm(ub,uba)

names(ubj) <- c("BARRIO","BUFFER","RBARRIO")
