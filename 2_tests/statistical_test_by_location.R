library(tidyverse)
library(dunn.test)

dataset <- read_csv("/Users/robsalasco/Dev/barrio-indicators-evaltool/data.csv",
                    col_types = cols(ID_W = col_character(),X1 = col_skip()))

dataset <- dataset %>% filter(!is.na(listo))
vars <- unique(dataset$codename)

stage_2_nonormal_test <- dataset %>% 
  slice_rows(c("codename")) %>% 
  by_slice(~tidy(with(.x,kruskal.test(listo~factor(LOCATION))))) %>%
  unnest() %>% select(codename,p.value,method)

for(x in vars){
  print(x)
  posthoc.kruskal.dunn.test(subset(dataset,codename==x)$listo,factor(subset(dataset,codename==x)$LOCATION), "bonferroni")
  #dunn.test(subset(dataset,codename==x)$listo,factor(subset(dataset,codename==x)$LOCATION),list=T,table = F,kw=F)
}

dataset %>% 
  slice_rows(c("LOCATION","codename")) %>% 
  by_slice(~tidy(shapiro.test(.x$listo)), .collate = "rows") %>% 
  mutate(normal=ifelse(p.value>0.05,1,0))

dataset %>%
  slice_rows(c("LOCATION", "codename")) %>%
  by_slice( ~ tidy(with(
    .x, wilcox.test(as.numeric(listo), as.numeric(manzb))
  )), .collate = "rows") %>% left_join(
    dataset %>%
      dplyr::group_by(codename, LOCATION) %>%
      dplyr::summarize(
        MEAN_BARRIO = mean(listo[manzb == T], na.rm = T),
        MEAN_BUFFER =
          mean(listo[manzb == F], na.rm = T),
        STD_BARRIO =
          sd(listo[manzb == T], na.rm = T),
        STD_BUFFER =
          sd(listo[manzb == F], na.rm = T),
        UNITS_BARRIO =
          length(listo[manzb == T]),
        UNITS_BUFFER =
          length(listo[manzb == F])
      ),
    by = c("LOCATION", "codename")
  )
