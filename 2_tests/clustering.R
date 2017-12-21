library(tidyverse)
library(Ckmeans.1d.dp)
library(ggrepel)
library(gridExtra)

# Database
dataset <- read_csv("/Users/robsalasco/Dev/barrio-indicators-evaltool/data.csv")
data <- dataset %>% filter(is.numeric(listo))
data1 <-  dataset %>% filter(codename=="avaluo" & manzb==T) %>% select(listo,d) %>% group_by(d) %>% dplyr::summarize(x=mean(as.numeric(listo),na.rm = T))
data1$x <- round(data1$x,2)
data1 <- data1 %>% filter(!is.na(x))

result1 <- Ckmeans.1d.dp(data1$x, 3)

ggplot(data = data1, aes(x = 1:nrow(data1), y = x, colour=as.factor(result1$cluster))) +
  geom_hline(yintercept=round(result1$centers,2), linetype="dashed", color = "black") +
  geom_point() + geom_text_repel(aes(label=d),size = 3)+
  labs(x = "index",colour="cluster",y="value") +
  scale_y_continuous(breaks = sort(c(seq(min(data1$x), max(data1$x), length.out=2), round(result1$centers,2)))) + ggtitle("Barrio")

data2 <-  dataset %>% filter(codename=="empl" & manzb==F) %>% select(listo,d) %>% group_by(d) %>% dplyr::summarize(x=mean(listo,na.rm = T))
data2$x <- round(data2$x,2)

result2 <- Ckmeans.1d.dp(data2$x, 3)

ggplot(data = data2, aes(x = 1:nrow(data2), y = x, colour=as.factor(result2$cluster))) +
  geom_hline(yintercept=round(result2$centers,2), linetype="dashed", color = "black") +
  geom_point() + geom_text_repel(aes(label=d),size = 3)+
  labs(x = "index",colour="cluster",y="value") +
  scale_y_continuous(breaks = sort(c(seq(min(data2$x), max(data2$x), length.out=2), round(result2$centers,2)))) + ggtitle("Buffer")
