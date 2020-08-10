
library("tidyverse")
dat <- read_csv("ranking_data2.csv")

dl <- dat %>% 
  select(-ends_with("order")) %>% 
  pivot_longer(cols = starts_with("event_set"), 
               names_to = "event", values_to = "response") %>% 
  filter(str_detect(response, "\\d")) %>% 
  mutate(set = str_extract(event, "\\d")) %>% 
  mutate(type = str_remove(event, "event_set\\d_")) %>% 
  select(-event)

dl2 <- dat %>% 
  select(X1, PROLIFIC_PID, condition, numeracy_score, ends_with("order")) %>% 
  pivot_longer(cols = starts_with("event_set"), 
               names_to = "event", values_to = "order") %>% 
  filter(!str_detect(order, "-")) %>%
  mutate(set = str_extract(event, "\\d")) %>%  
  select(-event)

dl <- dl %>% 
  left_join(dl2)

dl %>% 
  print(n = 20)

dl2 <- dl %>% 
  group_by(X1, PROLIFIC_PID, condition, set, numeracy_score) %>% 
  nest() 

dl2$data[[1]]

dl2 %>% 
  mutate(coherence_overall = map_dbl(data, my_function)) ### write my_function
