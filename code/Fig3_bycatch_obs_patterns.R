

#################################
### clean working environment ###
#################################
rm(list = ls())

### Load in packages ###
#######################
library(tidyverse)

### Read in data ###
####################
total_merge <- read.csv("data/confidential/processed/fig3_merge_obs_and_trip_data.csv")

#####################################################################
### Panel A (1) - Distance to shore and BPUE of sensitive species ###
#####################################################################

# add distance category to total_merge

total_merge_dist <- total_merge %>%
  mutate(dist_catg = ifelse(data_source == "CDFW 1980s",
                            cut(dist_km, breaks = seq(0,41,0.1), label = seq(0,40.9, 0.1), right = F) %>% as.character() %>% as.numeric(),
                            cut(dist_km, breaks = seq(0,80,1), label = seq(0, 79, 1), right = F) %>% as.character() %>% as.numeric()))


# Find out how many sets in each unique combination of data source and dist_catg
number_sets_dist <- total_merge_dist %>%
  group_by(data_source, dist_catg) %>%
  summarize(number_sets = n_distinct(set_id)) %>%
  unite (unique_id, c(data_source, dist_catg), sep = "-", remove = FALSE)

# Find out how many summed efforts in each unique combination of data source and dist_catg
number_effort_dist <- total_merge_dist %>%
  filter(!is.na(net_length_fa)) %>%
  filter(!is.na(soak_hr)) %>%
  select(set_id, net_length_fa, soak_hr, data_source, dist_catg) %>%
  # turn units from hrs*fa into day*miles
  mutate(effort = soak_hr*net_length_fa * (1/(880*24))) %>%
  filter(!duplicated(set_id)) %>%
  group_by(data_source, dist_catg) %>%
  summarize(number_efforts = sum(effort)) %>%
  unite (unique_id, c(data_source, dist_catg), sep = "-", remove = TRUE)

# Find out how many sealion bycatch in each unique combination of data source and dist_catg
number_sealion_dist <- total_merge_dist %>%
  filter(comm_name == "California sea lion") %>%
  group_by(data_source, dist_catg) %>%
  summarize(number_sealion = sum(n_discarded_total), set_sealion = n_distinct(set_id)) %>%
  unite (unique_id, c(data_source, dist_catg), sep = "-", remove = TRUE)

# Find out how many harbor seal bycatch in each unique combination of data source and dist_catg
number_seal_dist <- total_merge_dist %>%
  filter(comm_name == "Harbor seal") %>%
  group_by(data_source, dist_catg) %>%
  summarize(number_seal = sum(n_discarded_total), set_seal = n_distinct(set_id)) %>%
  unite (unique_id, c(data_source, dist_catg), sep = "-", remove = TRUE)

mammal_bycatch_dist <- merge(number_sets_dist, number_effort_dist, by = "unique_id", all.x = TRUE) %>%
  merge(number_sealion_dist, by = "unique_id", all.x = TRUE) %>%
  merge(number_seal_dist, by = "unique_id", all.x = TRUE) %>%
  mutate(bpue_sealion = number_sealion/number_efforts) %>%
  mutate(bpue_seal = number_seal/number_efforts) %>%
  mutate(proportion_sealion = set_sealion/number_sets) %>%
  mutate(proportion_seal = set_seal/number_sets) %>%
  mutate_all(~replace(., is.na(.), 0))

ggplot(data = mammal_bycatch_dist, aes(x=dist_catg, y = bpue_sealion, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  lims(y = c(0, 3), x = c(0, 20))
  

ggplot(data = mammal_bycatch_dist, aes(x=dist_catg, y = bpue_seal, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  lims(y = c(0, 1), x = c(0, 20))


ggplot(data = mammal_bycatch_dist, aes(x=dist_catg, y = proportion_sealion, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  lims(y = c(0, 0.2), x = c(0, 20))

ggplot(data = mammal_bycatch_dist, aes(x=dist_catg, y = proportion_seal, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  lims(y = c(0, 0.1), x = c(0, 20))
  
 


