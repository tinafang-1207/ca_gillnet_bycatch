
### clean working environment
rm(list = ls())

### Load in packages
library(tidyverse)

### read in data

# observer data
obs_2000 <- readRDS("data/confidential/original/SWFSC_set_net_observer_data.Rds")
obs_1980 <- readRDS("data/confidential/original/CDFW_1983_1989_gillnet_observer_data.Rds")

#trip data
trip_2000 <- readRDS("data/confidential/original/SWFSC_1990_2017_set_net_observer_trips.Rds")
trip_1980 <- readRDS("data/confidential/original/CDFW_1983_1989_gillnet_observer_set_info.Rds")


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
  

