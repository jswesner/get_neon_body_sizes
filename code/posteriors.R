library(tidyverse)
library(brms)
library(tidybayes)
library(ggthemes)

# mod_list = readRDS(file = "models/mod_list.rds")
mod_list = readRDS(file = "models/mod_list_macros_fish.rds")

posts_list = list()

for(i in 1:length(mod_list)){
  posts_list[[i]] = mod_list[[i]]$data %>% distinct(xmin, xmax) %>% mutate(no_m2 = 1) %>%
    mutate(model = list(mod_list[[i]]$data2),
           n_sizes = nrow(mod_list[[2]]$data)) %>% 
    add_epred_draws(mod_list[[i]])
}

all_posts = bind_rows(posts_list) %>% unnest_wider(model)
saveRDS(all_posts, file = "posteriors/all_posts_macros_fish.rds")


