

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

total_merge_all <- read.csv("data/confidential/processed/fig3_merge_obs_and_trip_data_all.csv")

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
  
################################################################### 
### Panel A (2) - Haul depth (fa) and BPUE of sensitive species ###
###################################################################

number_sets_depth <- total_merge %>%
  filter(!is.na(haul_depth_fa)) %>%
  group_by(data_source, haul_depth_fa) %>%
  summarize(number_sets = n_distinct(set_id)) %>%
  unite (unique_id, c(data_source, haul_depth_fa), sep = "-", remove = FALSE)

number_effort_depth <- total_merge %>%
  filter(!is.na(haul_depth_fa)) %>%
  filter(!is.na(net_length_fa)) %>%
  filter(!is.na(soak_hr)) %>%
  select(set_id, net_length_fa, soak_hr, data_source, haul_depth_fa) %>%
  # turn units from hrs*fa into day*miles
  mutate(effort = soak_hr*net_length_fa * (1/(880*24))) %>%
  filter(!duplicated(set_id)) %>%
  group_by(data_source, haul_depth_fa) %>%
  summarize(number_efforts = sum(effort)) %>%
  unite (unique_id, c(data_source, haul_depth_fa), sep = "-", remove = TRUE)

number_sealion_depth <- total_merge %>%
  filter(comm_name == "California sea lion") %>%
  group_by(data_source, haul_depth_fa) %>%
  summarize(number_sealion = sum(n_discarded_total), set_sealion = n_distinct(set_id)) %>%
  unite (unique_id, c(data_source, haul_depth_fa), sep = "-", remove = TRUE)

number_seal_depth <- total_merge %>%
  filter(comm_name == "Harbor seal") %>%
  group_by(data_source, haul_depth_fa) %>%
  summarize(number_seal = sum(n_discarded_total), set_seal = n_distinct(set_id)) %>%
  unite (unique_id, c(data_source, haul_depth_fa), sep = "-", remove = TRUE)

mammal_bycatch_depth <- merge(number_sets_depth, number_effort_depth, by = "unique_id", all.x = TRUE) %>%
  merge(number_sealion_depth, by = "unique_id", all.x = TRUE) %>%
  merge(number_seal_depth, by = "unique_id", all.x = TRUE) %>%
  mutate(bpue_sealion = number_sealion/number_efforts) %>%
  mutate(bpue_seal = number_seal/number_efforts) %>%
  mutate(proportion_sealion = set_sealion/number_sets) %>%
  mutate(proportion_seal = set_seal/number_sets) %>%
  mutate_all(~replace(., is.na(.), 0))

base_theme <- theme(axis.text=element_text(size=6),
                    axis.text.y = element_text(angle = 90, hjust = 0.5),
                    axis.title=element_text(size=7),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=7),
                    plot.tag =element_text(size=8),
                    plot.title=element_blank(),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"))

g1_depth <- ggplot(data = mammal_bycatch_depth, aes(x=haul_depth_fa, y = bpue_sealion, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  lims(y = c(0, 2), x = c(0, 50)) +
  theme_bw() + base_theme
g1_depth

g2_depth <- ggplot(data = mammal_bycatch_depth, aes(x=haul_depth_fa, y = bpue_seal, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  lims(y = c(0, 0.5), x = c(0, 50)) +
  theme_bw() + base_theme
g2_depth

g3_depth <- ggplot(data = mammal_bycatch_depth, aes(x=haul_depth_fa, y = proportion_sealion, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  lims(y = c(0, 0.2), x = c(0, 50)) +
  theme_bw() +base_theme
g3_depth

g4_depth <- ggplot(data = mammal_bycatch_depth, aes(x=haul_depth_fa, y = proportion_seal, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  lims(y = c(0, 0.1), x = c(0, 50))+
  theme_bw() +base_theme
g4_depth

#################################################################
### Panel A (3) - Haul latitude and BPUE of sensitive species ###
#################################################################

total_merge_latitude <- total_merge %>%
  mutate(lat_catg = cut(haul_lat_dd, breaks = seq(32.4, 35.7,0.1), 
                         label = seq(32.4, 35.6, 0.1), right = F) %>% as.character() %>% as.numeric())

number_sets_lat <- total_merge_latitude %>%
  group_by(data_source, lat_catg) %>%
  summarize(number_sets = n_distinct(set_id)) %>%
  unite (unique_id, c(data_source, lat_catg), sep = "-", remove = FALSE)

number_effort_lat <- total_merge_latitude %>%
  filter(!is.na(net_length_fa)) %>%
  filter(!is.na(soak_hr)) %>%
  select(set_id, net_length_fa, soak_hr, data_source, lat_catg) %>%
  # turn units from hrs*fa into day*miles
  mutate(effort = soak_hr*net_length_fa * (1/(880*24))) %>%
  filter(!duplicated(set_id)) %>%
  group_by(data_source, lat_catg) %>%
  summarize(number_efforts = sum(effort)) %>%
  unite (unique_id, c(data_source, lat_catg), sep = "-", remove = TRUE)

number_sealion_lat <- total_merge_latitude %>%
  filter(comm_name == "California sea lion") %>%
  group_by(data_source, lat_catg) %>%
  summarize(number_sealion = sum(n_discarded_total), set_sealion = n_distinct(set_id)) %>%
  unite (unique_id, c(data_source, lat_catg), sep = "-", remove = TRUE)

number_seal_lat <- total_merge_latitude %>%
  filter(comm_name == "Harbor seal") %>%
  group_by(data_source, lat_catg) %>%
  summarize(number_seal = sum(n_discarded_total), set_seal = n_distinct(set_id)) %>%
  unite (unique_id, c(data_source, lat_catg), sep = "-", remove = TRUE)

mammal_bycatch_lat <- merge(number_sets_lat, number_effort_lat, by = "unique_id", all.x = TRUE) %>%
  merge(number_sealion_lat, by = "unique_id", all.x = TRUE) %>%
  merge(number_seal_lat, by = "unique_id", all.x = TRUE) %>%
  mutate(bpue_sealion = number_sealion/number_efforts) %>%
  mutate(bpue_seal = number_seal/number_efforts) %>%
  mutate(proportion_sealion = set_sealion/number_sets) %>%
  mutate(proportion_seal = set_seal/number_sets) %>%
  mutate_all(~replace(., is.na(.), 0))

base_theme <- theme(axis.text=element_text(size=6),
                    axis.text.y = element_text(angle = 90, hjust = 0.5),
                    axis.title=element_text(size=7),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=7),
                    plot.tag =element_text(size=8),
                    plot.title=element_blank(),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"))

g1_lat <- ggplot(data = mammal_bycatch_lat, aes(x=lat_catg, y = bpue_sealion, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  lims(y = c(0, 2), x = c(32, 35)) +
  theme_bw() + base_theme
g1_lat

g2_lat <- ggplot(data = mammal_bycatch_lat, aes(x= lat_catg, y = bpue_seal, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  lims(y = c(0, 0.4), x = c(32, 35)) +
  theme_bw() + base_theme
g2_lat

g3_lat <- ggplot(data = mammal_bycatch_lat, aes(x=lat_catg, y = proportion_sealion, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  lims(y = c(0, 0.25), x = c(32, 35)) +
  theme_bw() +base_theme
g3_lat

g4_lat <- ggplot(data = mammal_bycatch_lat, aes(x=lat_catg, y = proportion_seal, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  lims(y = c(0, 0.1), x = c(32,35))+
  theme_bw() +base_theme
g4_lat


##############################################################
### Panel A (4) - Julian Day and BPUE of sensitive species ###
##############################################################

number_sets_jd <- total_merge %>%
  group_by(data_source, julian_day) %>%
  summarize(number_sets = n_distinct(set_id)) %>%
  unite (unique_id, c(data_source, julian_day), sep = "-", remove = FALSE)

number_effort_jd <- total_merge %>%
  filter(!is.na(net_length_fa)) %>%
  filter(!is.na(soak_hr)) %>%
  select(set_id, net_length_fa, soak_hr, data_source, julian_day) %>%
  # turn units from hrs*fa into day*miles
  mutate(effort = soak_hr*net_length_fa * (1/(880*24))) %>%
  filter(!duplicated(set_id)) %>%
  group_by(data_source, julian_day) %>%
  summarize(number_efforts = sum(effort)) %>%
  unite (unique_id, c(data_source, julian_day), sep = "-", remove = TRUE)

number_sealion_jd <- total_merge %>%
  filter(comm_name == "California sea lion") %>%
  group_by(data_source, julian_day) %>%
  summarize(number_sealion = sum(n_discarded_total), set_sealion = n_distinct(set_id)) %>%
  unite (unique_id, c(data_source, julian_day), sep = "-", remove = TRUE)

number_seal_jd <- total_merge %>%
  filter(comm_name == "Harbor seal") %>%
  group_by(data_source, julian_day) %>%
  summarize(number_seal = sum(n_discarded_total), set_seal = n_distinct(set_id)) %>%
  unite (unique_id, c(data_source, julian_day), sep = "-", remove = TRUE)

mammal_bycatch_jd <- merge(number_sets_jd, number_effort_jd, by = "unique_id", all.x = TRUE) %>%
  merge(number_sealion_jd, by = "unique_id", all.x = TRUE) %>%
  merge(number_seal_jd, by = "unique_id", all.x = TRUE) %>%
  mutate(bpue_sealion = number_sealion/number_efforts) %>%
  mutate(bpue_seal = number_seal/number_efforts) %>%
  mutate(proportion_sealion = set_sealion/number_sets) %>%
  mutate(proportion_seal = set_seal/number_sets) %>%
  mutate_all(~replace(., is.na(.), 0))

base_theme <- theme(axis.text=element_text(size=6),
                    axis.text.y = element_text(angle = 90, hjust = 0.5),
                    axis.title=element_text(size=7),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=7),
                    plot.tag =element_text(size=8),
                    plot.title=element_blank(),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"))


g1_jd <- ggplot(data = mammal_bycatch_jd, aes(x=julian_day, y = bpue_sealion, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  lims(y = c(0, 10)) +
  theme_bw() + base_theme

g1_jd

g2_jd <- ggplot(data = mammal_bycatch_jd, aes(x= julian_day, y = bpue_seal, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  lims(y = c(0, 2)) +
  theme_bw() + base_theme

g2_jd

g3_jd <- ggplot(data = mammal_bycatch_jd, aes(x=julian_day, y = proportion_sealion, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  #lims(y = c(0, 0.25), x = c(32, 35)) +
  theme_bw() +base_theme

g3_jd

g4_jd <- ggplot(data = mammal_bycatch_jd, aes(x=julian_day, y = proportion_seal, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  #lims(y = c(0, 0.25), x = c(32, 35)) +
  theme_bw() +base_theme

g4_jd

