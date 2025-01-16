library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(poweRlaw)

dat_2024 = readRDS(file = "data/dat_2024.rds")

dat_inverts_list = dat_2024 %>% 
  group_by(sample_id) %>% 
  sample_n(5000, weight = no_m2, replace = T) %>% 
  group_by(sample_id) %>% group_split()

xmin_inverts_list = list()

for(i in 1:length(dat_inverts_list)){
  powerlaw = conpl$new(dat_inverts_list[[i]]$dw)
  xmin_inverts_list[[i]] = tibble(xmin_clauset = estimate_xmin(powerlaw)$xmin,
                                  sample_id = unique(dat_inverts_list[[i]]$sample_id))
}

xmins_inverts_clauset = bind_rows(xmin_inverts_list)

dat_2024_clauset_xmins = dat_2024 %>% left_join(xmins_inverts_clauset) %>% 
  group_by(sample_id) %>% 
  filter(dw >= xmin_clauset) %>%
  mutate(xmin = xmin_clauset,
         xmax = max(dw)) %>% 
  mutate(year = as.integer(year(collect_date)))


# check for low sample sizes
dat_2024_clauset_xmins %>% 
  group_by(sample_id) %>% 
  tally() %>% 
  arrange(n)

# save with different year cutoffs
saveRDS(dat_2024_clauset_xmins, file = "data/dat_2024_clauset.rds")
saveRDS(dat_2024_clauset_xmins %>% filter(year <= 2022), 
        file = "data/dat_2022_clauset.rds")
saveRDS(dat_2024_clauset_xmins %>% filter(year >= 2022), 
        file = "data/dat_20232024_clauset.rds")



