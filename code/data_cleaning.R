
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


### Format and combine trip data

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


### Format and combine observer data

obs_1980_pre_join <- 

