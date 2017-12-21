library(tidyverse)
library(dunn.test)

dataset <- read_csv("/Users/robsalasco/Dev/barrio-indicators-evaltool/data.csv",
                    col_types = cols(ID_W = col_character(),X1 = col_skip()))

test1 <- dataset %>% filter(!is.na(listo))
vars_test1 <- unique(test1$codename)

stage_2_nonormal_test <- test1 %>% 
  slice_rows(c("codename")) %>% 
  by_slice(~tidy(with(.x,kruskal.test(listo~factor(LOCATION))))) %>%
  unnest() %>% select(codename,p.value,method)

log <- capture.output({
  test1_result <- lapply(vars_test1, function(x){dunn.test(subset(test1,codename==x)$listo,factor(subset(test1,codename==x)$LOCATION),list=T,table = F,kw=F)})
})




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
