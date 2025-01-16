library(tidyverse)
library(brms)
library(tidybayes)
library(ggthemes)

all_posts = readRDS(file = "posteriors/all_posts.rds") %>% mutate(sample_id = id)# macros only
# all_posts = readRDS(file = "posteriors/all_posts_macros_fish.rds")

old_data = readRDS("C:/Users/jeff.wesner/OneDrive - The University of South Dakota/USD/Github Projects/neon_size_spectra-slim/data/derived_data/dat_inverts_clauset_xmins.rds")
temp = old_data %>% ungroup %>% distinct(site_id, mat_s, log_om_s, log_gpp_s)

all_posts %>% 
  group_by(sample_id, site_id, macrodate) %>% 
  median_qi(.epred) %>% 
  ggplot(aes(x = macrodate)) +
  # geom_point(aes(y = .epred)) +
  geom_pointrange(aes(y = .epred, ymin = .lower, ymax = .upper)) +
  geom_line(aes(y = .epred, group = site_id)) +
  facet_wrap(~site_id) +
  geom_hline(yintercept = -2)


streamsites=c("HOPB", "LEWI", "POSE", "CUPE",
              "GUIL", "KING", "MCDI", "LECO",
              "WALK", "MAYF", "ARIK", "BLUE",
              "PRIN", "BLDE", "COMO", "WLOU",
              "SYCA", "REDB", "MART", "MCRA",
              "BIGC", "TECR", "OKSR", "CARI")

time_series = all_posts %>% 
  group_by(sample_id, site_id, macrodate) %>% 
  median_qi(.epred) %>% 
  filter(.epred > -4) %>% 
  filter(site_id %in% streamsites) %>% 
  mutate(macrodate = as.Date(macrodate)) %>% 
  ggplot(aes(x = macrodate)) +
  # geom_point(aes(y = .epred)) +
  geom_linerange(aes(y = .epred, ymin = .lower, ymax = .upper),
                 linewidth = 0.01) +
  geom_point(aes(y = .epred), size = 0.1)+
  geom_line(aes(y = .epred, group = site_id), linewidth = 0.2) +
  facet_wrap(~site_id) +
  geom_hline(yintercept = -2.08, linewidth = 0.1) +
  theme_default() +
  labs(y = "\u03bb",
       x = "Date") +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  theme(text = element_text(size = 7),
        axis.text.x = element_text(angle = 90))

ggsave(time_series, file = "plots/time_series.jpg", width = 4, height = 5)

all_posts %>% 
  group_by(site_id, macrodate) %>% 
  filter(macrodate <= as.Date("2022-01-01")) %>% 
  median_qi(.epred) %>% 
  left_join(temp) %>% 
  ggplot(aes(x = mat_s, y = .epred)) +
  geom_point() +
  geom_smooth(method = lm)


# old_lambdas = list()
# old_data_list = old_data %>% group_by(site_id, date, sample_id, year) %>% 
#   group_split()
# 
# for(i in 1:length(old_data_list)){
#   old_lambdas[[i]] = update(brm_dummy, newdata = old_data_list[[i]], 
#                             data2 = list(sample_id = unique(old_data_list[[i]]$sample_id),
#                                          site_id = unique(old_data_list[[i]]$site_id)), 
#                             iter = 1000, chains = 1)
# }
# 
# saveRDS(old_lambdas, file = "models/old_lambdas.rds")
# 
# posts_list_old = list()
# 
# for(i in 1:length(old_lambdas)){
#   posts_list_old[[i]] = old_lambdas[[i]]$data %>% distinct(xmin, xmax) %>% mutate(no_m2 = 1) %>%
#     mutate(model = list(old_lambdas[[i]]$data2)) %>% 
#     add_epred_draws(old_lambdas[[i]])
# }
# 
# all_posts_old = bind_rows(posts_list_old) %>% unnest_wider(model)
# saveRDS(all_posts_old, file = "posteriors/all_posts_old.rds")
all_posts_old = readRDS(file = "posteriors/all_posts_old.rds")


bind_rows(all_posts_old %>% mutate(source = "old", id = sample_id),
          all_posts %>% mutate(source = "new")) %>% 
  group_by(id, source, site_id) %>% 
  median_qi(.epred) %>% 
  left_join(temp) %>% 
  ggplot(aes(x = mat_s, y = .epred)) + 
  geom_point(aes(color = source)) +
  facet_wrap(~source) +
  geom_smooth(method = lm)

test_dat = all_posts_old %>% 
  group_by(sample_id, site_id) %>% 
  median_qi(.epred) %>% 
  left_join(temp) %>% 
  filter(!is.na(mat_s))

test_mod = lm(.epred ~ mat_s, data = test_dat)
summary(test_mod)



