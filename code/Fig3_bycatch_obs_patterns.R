
### clean working environment ###
rm(list = ls())

### Load in packages ###

### Read in data ###

### Panel A - Distance to shore and Bycatch ratio/BPUE of sensitive species ###

### California Sealion ###
number_sets_dist <- total_merge_dist %>%
  group_by(data_source, dist_catg) %>%
  summarize(number_sets = n_distinct(set_id)) %>%
  unite (unique_id, c(data_source, dist_catg), sep = "-", remove = FALSE)

number_sealion_dist <- total_merge_dist %>%
  filter(comm_name == "California sea lion") %>%
  group_by(data_source, dist_catg) %>%
  summarize(number_sealion = sum(n_discarded_total)) %>%
  unite (unique_id, c(data_source, dist_catg), sep = "-", remove = TRUE)

number_effort_dist <- total_merge_dist %>%
  filter(!is.na(net_length_fa)) %>%
  filter(!is.na(soak_hr)) %>%
  select(set_id, net_length_fa, soak_hr, data_source, dist_catg) %>%
  mutate(effort = soak_hr*net_length_fa) %>%
  filter(!duplicated(set_id)) %>%
  group_by(data_source, dist_catg) %>%
  summarize(number_efforts = sum(effort)) %>%
  unite (unique_id, c(data_source, dist_catg), sep = "-", remove = TRUE)



sealion_bycatch_dist <- merge(number_sets_dist, number_sealion_dist, by = "unique_id", all.x = TRUE) %>%
  merge(number_effort_dist, by = "unique_id", all.x = TRUE) %>%
  filter(!is.na(dist_catg)) %>%
  mutate(number_sealion = ifelse(is.na(number_sealion), 0, number_sealion)) %>%
  mutate(bpue_sealion = number_sealion/number_efforts) %>%
  mutate(proportion_set = number_sealion/number_sets)

ggplot(data = sealion_bycatch_dist, aes(x=dist_catg, y = bpue_sealion, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  lims(y = c(0, 3*10^-5), x = c(0, 20))


  
 


