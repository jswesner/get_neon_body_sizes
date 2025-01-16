library(tidyverse)
library(brms)
library(tidybayes)
library(ggthemes)
library(isdbayes)

brm_dummy <- readRDS("models/brm_dummy.rds")
dat_inverts_clauset_xmins = readRDS(file = "data/dat_inverts_clauset_xmins.rds") %>% 
  group_by(collect_date, site_id) %>% 
  mutate(id = cur_group_id())


dat_list = dat_inverts_clauset_xmins %>% group_by(collect_date,site_id, id) %>% 
  group_split()


mod_list = list()

for(i in 1:length(dat_list)){
  mod_list[[i]] = update(brm_dummy, newdata = dat_list[[i]], 
                         data2 = list(id = unique(dat_list[[i]]$id),
                                      collect_date = unique(dat_list[[i]]$collect_date),
                                      site_id = unique(dat_list[[i]]$site_id)), 
                         iter = 1000, chains = 1)
}


saveRDS(mod_list, file = "models/mod_list.rds")


# fish and macros --------------------------------------------------------------------

dat_macro_fish_clauset_xmins = readRDS(file = "data/dat_macro_fish_clauset_xmins.rds")


dat_list = dat_macro_fish_clauset_xmins %>% group_by(site_id, sample_id, year, macrodate) %>% 
  group_split()


mod_list = list()

brm_dummy = readRDS("models/brm_dummy.rds")

for(i in 1:length(dat_list)){
  mod_list[[i]] = update(brm_dummy, newdata = dat_list[[i]], 
                         data2 = list(sample_id = unique(dat_list[[i]]$sample_id),
                                      macrodate = unique(dat_list[[i]]$macrodate),
                                      site_id = unique(dat_list[[i]]$site_id)), 
                         iter = 1000, chains = 1)
}


saveRDS(mod_list, file = "models/mod_list_macros_fish.rds")

