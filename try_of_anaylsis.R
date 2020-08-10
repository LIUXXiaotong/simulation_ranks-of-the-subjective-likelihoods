
library("tidyverse")
dat <- read_csv("ranking_data2.csv")
source("analysis_functions.R")

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
  select(-event) %>% 
  separate(order, paste0("order", 1:3))

dl <- dl %>% 
  left_join(dl2)

dl %>% 
  print(n = 20)

dl_nest <- dl %>% 
  group_by(X1, PROLIFIC_PID, condition, set, numeracy_score) %>% 
  nest() 

calc_coh_marg_conj(dl_nest$data[[4]])


dl_nest %>% 
  mutate(coh_marg_conj = map_dbl(data, calc_coh_marg_conj)) ### write my_function
