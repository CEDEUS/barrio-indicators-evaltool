library(tidyverse)

# Databases
barrios_m <- readRDS("Data/barrios_merged.RDS")
manzbarr <- readRDS("Data/Manzanasbarriospiloto.RDS")
dataset <- read_csv("/Users/robsalasco/Dev/barrio-indicators-evaltool/data.csv")

# Total Units by geo
units <-
  right_join(
    barrios_m@data %>% dplyr::count(BARRIO),
    manzbarr %>% dplyr::count(BARRIO),
    by = "BARRIO"
  )

# Units by variable
dataset %>% dplyr::filter(codename=="empl") %>%
  dplyr::group_by(BARRIO) %>%
  dplyr::summarize(
    MEAN_BARRIO=mean(listo[manzb==T],na.rm = T),
    MEAN_BUFFER=mean(listo[manzb==F],na.rm = T),
    STD_BARRIO=sd(listo[manzb==T],na.rm = T),
    STD_BUFFER=sd(listo[manzb==F],na.rm = T),    
    UNITS_BARRIO=length(listo[manzb==T]),
    UNITS_BUFFER=length(listo[manzb==F])
  )


# Units by barrio all variables 
dataset %>%
  dplyr::group_by(codename, d) %>%
  dplyr::summarize(
    MEAN_BARRIO=mean(listo[manzb==T],na.rm = T),
    MEAN_BUFFER=mean(listo[manzb==F],na.rm = T),
    STD_BARRIO=sd(listo[manzb==T],na.rm = T),
    STD_BUFFER=sd(listo[manzb==F],na.rm = T),    
    UNITS_BARRIO=length(listo[manzb==T]),
    UNITS_BUFFER=length(listo[manzb==F])
  )

# Units by city all variables long format
dataset %>%
  dplyr::group_by(codename, CIUDAD, manzb) %>%
  dplyr::summarize(
    MEAN=mean(listo,na.rm = T),
    STD=sd(listo,na.rm = T),
    UNITS=length(listo)
  )

#Units by city all variables wide format
dataset %>%
  dplyr::group_by(codename, CIUDAD) %>%
  dplyr::summarize(
    MEAN_BARRIO=mean(listo[manzb==T],na.rm = T),
    MEAN_BUFFER=mean(listo[manzb==F],na.rm = T),
    STD_BARRIO=sd(listo[manzb==T],na.rm = T),
    STD_BUFFER=sd(listo[manzb==F],na.rm = T),    
    UNITS_BARRIO=length(listo[manzb==T]),
    UNITS_BUFFER=length(listo[manzb==F])
  )
