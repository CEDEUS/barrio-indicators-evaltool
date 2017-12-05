library(tidyverse)

barrios_m <- readRDS("Data/barrios_merged.RDS")
manzbarr <- readRDS("Data/Manzanasbarriospiloto.RDS")

# Unit counter

units <-
  right_join(
    barrios_m@data %>% group_by(BARRIO) %>% summarise(BUFFER = n()),
    manzbarr %>% group_by(BARRIO) %>% summarise(RBARRIO = n()),
    by = "BARRIO"
  )

units

