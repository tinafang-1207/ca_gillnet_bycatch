
### clean working environment
rm(list = ls())

### Load in packages
library(tidyverse)
library(sf)

### read in data

# observer data
obs_2000 <- readRDS("data/confidential/original/SWFSC_set_net_observer_data.Rds")
obs_1980 <- readRDS("data/confidential/original/CDFW_1983_1989_gillnet_observer_data.Rds")

#trip data
trip_2000 <- readRDS("data/confidential/original/SWFSC_1990_2017_set_net_observer_trips.Rds")
trip_1980 <- readRDS("data/confidential/original/CDFW_1983_1989_gillnet_observer_set_info.Rds")

# spatial data
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")

usa_ca <- usa %>% filter(name == "California")


### Format and combine trip data from 1980-2000

trip_1980_pre_join <- trip_1980 %>%
  select(date, vessel_id, set_id, port_depart, port_landing, lat_dd, long_dd, duration, net_type, net_length_fa, net_depth, mesh_size1_in) %>%
  rename(port_return = port_landing, haul_lat_dd = lat_dd, haul_long_dd = long_dd, haul_depth_fa = net_depth, net_mesh_size_in = mesh_size1_in) %>%
  #format soak hours
  separate("duration", c("hours", "minutes", "seconds"), sep = " ") %>%
  mutate(hours = gsub("H", "", hours)) %>%
  mutate(minutes = gsub("M", "", minutes)) %>%
  select(-seconds) %>%
  mutate(hours = as.numeric(hours)) %>%
  mutate(minutes = as.numeric(minutes)) %>%
  mutate(minutes = minutes/60) %>%
  mutate(hours = hours + minutes) %>%
  select(-minutes) %>%
  mutate_if(is.numeric, round, digits = 4) %>%
  rename(soak_hr = hours)

trip_2000_pre_join <- trip_2000 %>%
  select(date_haul1, vessel_plate, set_id, port_depart, port_return, haul_lat_dd, haul_long_dd, soak_hr, haul_depth_fa, net_type, net_mesh_size_in, net_mesh_panel_length_fathoms) %>%
  rename(date = date_haul1, vessel_id = vessel_plate, net_length_fa = net_mesh_panel_length_fathoms)

trip_merge <- rbind(trip_1980_pre_join, trip_2000_pre_join)


### Format and combine observer data from 1980-2000

obs_1980_pre_join <- obs_1980 %>%
  mutate(n_retained = n_kept + n_kept_sublegal + n_sold) %>%
  mutate(n_discarded_unknown = 0) %>%
  mutate(n_discarded_total = n_discarded_alive + n_discarded_dead + n_discarded_unknown) %>%
  mutate(data_source = "CDFW 1980s") %>%
  select(-date, -vessel_id, -spp_code_chr, -n_kept, -n_kept_sublegal, -n_sold, -m_file_link, -s_file_link)

obs_2000_pre_join <- obs_2000 %>%
  mutate(n_caught = ifelse(!is.na(condition_code), 1, n_caught)) %>%
  mutate(n_kept = ifelse(!is.na(condition_code), 0, n_kept)) %>%
  mutate(n_returned_alive = ifelse(condition_code == "A"|condition_code == "I", 1, 0)) %>%
  mutate(n_returned_dead = ifelse(condition_code == "D", 1, 0)) %>%
  mutate(n_returned_unknown = ifelse(condition_code == "U", 1, 0)) %>%
  mutate(data_source = "SWFSC 2000s") %>%
  rename(n_retained = n_kept, n_discarded_dead = n_returned_dead, n_discarded_alive = n_returned_alive, n_discarded_unknown = n_returned_unknown) %>%
  mutate(n_discarded_total = n_discarded_alive + n_discarded_dead + n_discarded_unknown) %>%
  select(-trip_id, -spp_code, -comm_name_orig, -sci_name, -mammal_damage_yn,-n_damaged_mammals, -condition_code, -condition, -sex, -tag_yn)
  
obs_merge <- rbind(obs_1980_pre_join, obs_2000_pre_join)%>%
  select(set_num,set_id,comm_name, n_caught, n_retained, n_damaged, n_discarded_dead, n_discarded_alive, n_discarded_unknown, n_discarded_total, data_source)

# create datasheet with unique species name

species <- as.data.frame(sort(unique(obs_merge$comm_name)))

write.table(species, file = "data/species_key.csv", row.names = F, sep = ",")

species_key <- read.csv(file = "data/species_key_final.csv")

obs_merge_species <- merge(obs_merge, species_key, by = "comm_name") %>%
  mutate(n_retained = ifelse(is.na(n_retained), 0, n_retained)) %>%
  mutate(n_caught = ifelse(is.na(n_caught), 0, n_caught))

obs_merge_species_bycatch <- obs_merge_species %>%
  group_by(comm_name, type) %>%
  summarize(n_caught_species = sum(n_caught), n_retained_species = sum(n_retained)) %>%
  mutate(n_bycatch_species = n_caught_species - n_retained_species)

### Make bar charts for sensitive species

base_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_text(size=8),
                    axis.title.x=element_blank(),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=8),
                    strip.text=element_text(size=8),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"))

# marine mammal
ggplot(data = obs_merge_species_bycatch %>%filter(type == "mammal"), aes(x = reorder(comm_name, -n_bycatch_species), y = n_bycatch_species)) +
  geom_bar(stat = "identity", color = "grey30", fill = "grey30", lwd = 0.1) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(x = "Species", y = "Number of Bycatch") +
  theme_bw() + base_theme

#Sea bird
ggplot(data = obs_merge_species_bycatch %>%filter(type == "bird"), aes(x = reorder(comm_name, -n_bycatch_species), y = n_bycatch_species)) +
  geom_bar(stat = "identity", color = "grey30", fill = "grey30", lwd = 0.1) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(x = "Species", y = "Number of Bycatch") +
  theme_bw() + base_theme

#Turtle
ggplot(data = obs_merge_species_bycatch %>%filter(type == "turtle"), aes(x = reorder(comm_name, -n_bycatch_species), y = n_bycatch_species)) +
  geom_bar(stat = "identity", color = "grey30", fill = "grey30", lwd = 0.1) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(x = "Species", y = "Number of Bycatch") +
  theme_bw() + base_theme


ggplot(data = obs_merge_species_bycatch, aes(x = reorder(comm_name, -n_bycatch_species), y = n_bycatch_species)) +
  geom_bar(stat = "identity", color = "grey30", fill = "grey30", lwd = 0.1) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(x = "Species", y = "Number of Bycatch") +
  facet_wrap(.~type, scales = "free") +
  theme_bw() + base_theme
  
# Merge observer data with trip data
 
total_merge <- merge(obs_merge_species, trip_merge, by = "set_id")

total_merge_bpue <- total_merge %>%
  filter(!is.na(haul_lat_dd))

# set up projection to utm11

utm11 <- "+proj=utm +zone=11 +datum=NAD83"

# Convert fishing location data to sp
total_xy_sp <- total_merge_bpue %>%
  #Convert to sf
  st_as_sf(coords = c("haul_long_dd", "haul_lat_dd"), crs = st_crs(usa_ca), remove = F) %>%
  #Convert to SP
  as_Spatial()

total_xy_sp_utm <- total_xy_sp %>%
  sp::spTransform(utm11)

# Convert usa_ca to sp
usa_ca_sp <- usa_ca %>%
  st_transform(crs = utm11) %>%
  as_Spatial()

# Calculate distance of each point to ca shapefile
dist_mat <- rgeos::gDistance(total_xy_sp_utm, usa_ca_sp, byid = T)

#format distance matrix
dist_df <- dist_mat %>%
  as.data.frame() %>%
  gather(key = "row_id", value = "dist_m", 1:ncol(.)) %>%
  mutate(set_id = total_xy_sp_utm$set_id) %>%
  select(-row_id) %>%
  select(set_id, dist_m) %>%
  unique()

# Add distance matrix back to dataframe
total_merge_dist <- left_join(total_merge_bpue, dist_df, by = "set_id") %>%
  mutate(dist_km = dist_m/1000) %>%
  mutate(dist_catg = cut(dist_km, breaks = seq(0, 75,0.5), 
                         label = seq(0, 74.5, 0.5), right = F) %>% as.character() %>% as.numeric())

# California sea lion
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


  
 


