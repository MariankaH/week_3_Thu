library(tidyverse)
library(lubridate)
accidents <- readRDS("data/accidents.rds")

new_sum <- accidents %>% group_by(day_of_week) %>% 
  summarise(sum(vehicles))

filter_acc <- accidents %>% 
  select(day_of_week, time, severity, vehicles, id) %>% 
  mutate(weekends = case_when(day_of_week == "Saturday" | day_of_week == "Sunday" ~ "Weekend", day_of_week == "Monday" | day_of_week == "Tuesday" | day_of_week == "Wednesday"| day_of_week == "Thursday" | day_of_week == "Friday" ~ "Weekday") )  

filter_acc %>% 
  ggplot(aes(x = time, fill = severity)) +
  scale_color_manual(values = c("purple", "lightblue", "yellow"), aesthetics = c("colour", "fill")) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ weekends)
