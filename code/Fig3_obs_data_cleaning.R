
#################################################################################################################
### product: merged table of 1980s and 2000s obs and trip data that used to make figure 3 (observer data only, no prediction)
#################################################################################################################

#################################
### clean working environment ###
#################################

rm(list = ls())


########################
### Load in packages ###
########################

library(tidyverse)
library(sf)

####################
### read in data ###
####################

# observer data
obs_2000 <- readRDS("data/confidential/original/SWFSC_set_net_observer_data.Rds")
obs_1980 <- readRDS("data/confidential/original/CDFW_1983_1989_gillnet_observer_data.Rds")

# species key
species_key <- read.csv("data/species_key_final.csv")

#trip data
trip_2000 <- readRDS("data/confidential/original/SWFSC_1990_2017_set_net_observer_trips.Rds")

trip_1980 <- readRDS("data/confidential/original/CDFW_1983_1989_gillnet_observer_set_info.Rds")

trip_1980_lat_count <- trip_1980 %>%
  count(lat_dd, long_dd)

# spatial data
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")

usa_ca <- usa %>% filter(name == "California")

bathy_200fa <- raster::raster("data/gis_data/200mEEZ_BathyGrids/bd200fa_v2i")

bathy_200fa_WGS84 <- raster::projectRaster(bathy_200fa, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

######################
### Data Wrangling ###
######################

# Format and combine trip data from 1980s and 2000s

# select variables and format soak hours in 1980s data
trip_1980_soak_hours <- trip_1980 %>%
  #select columns to combine and rename them
  select(date, vessel_id, set_id, port_depart, port_landing, lat_dd, long_dd, duration, net_type, net_length_fa, mesh_size1_in) %>%
  rename(port_return = port_landing, haul_lat_dd = lat_dd, haul_long_dd = long_dd, net_mesh_size_in = mesh_size1_in) %>%
  #format soak hours
  separate("duration", c("hours", "minutes", "seconds"), sep = " ") %>%
  filter(!is.na(seconds)) %>%
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

# correct depth in 1980s data
trip_1980_pre_join_xy <- trip_1980_soak_hours %>%
  select(haul_long_dd, haul_lat_dd)

trip_1980_depth_mat <- raster::extract(bathy_200fa_WGS84, trip_1980_pre_join_xy)

trip_1980_depth_df <- trip_1980_depth_mat %>%
  as.data.frame() %>%
  gather(key = "row_id", value = "haul_depth_fa", 1:ncol(.)) %>%
  mutate(row_id = trip_1980_soak_hours$set_id) %>%
  rename(set_id = row_id)

trip_1980_pre_join <- merge(trip_1980_soak_hours, trip_1980_depth_df, by = "set_id") %>%
  mutate(haul_depth_fa = abs(haul_depth_fa)) %>%
  mutate(haul_depth_fa = round(haul_depth_fa, digits = 0))

# select and rename variables in 2000s data
trip_2000_pre_join <- trip_2000 %>%
  #select columns to combine and rename them
  select(date_haul1, vessel_plate, set_id, port_depart, port_return, haul_lat_dd, haul_long_dd, soak_hr, haul_depth_fa, net_type, net_mesh_size_in, net_mesh_panel_length_fathoms) %>%
  rename(date = date_haul1, vessel_id = vessel_plate, net_length_fa = net_mesh_panel_length_fathoms)


trip_merge <- rbind(trip_1980_pre_join, trip_2000_pre_join)


# Format and combine observer data of 1980s and 2000s

# select and format observer data in 1980s
obs_1980_pre_join <- obs_1980 %>%
  mutate(n_retained = n_kept + n_kept_sublegal + n_sold) %>%
  # add column of discarded_unknown in consistency with observer data after 2000s
  mutate(n_discarded_unknown = 0) %>%
  mutate(n_discarded_total = n_discarded_alive + n_discarded_dead + n_discarded_unknown) %>%
  # add data source column
  mutate(data_source = "CDFW 1980s") %>%
  select(-date, -vessel_id, -spp_code_chr, -n_kept, -n_kept_sublegal, -n_sold, -m_file_link, -s_file_link)

# select and format observer data in 2000s
obs_2000_pre_join <- obs_2000 %>%
  # format the capture/kept data of sensitive species
  mutate(n_caught = ifelse(!is.na(condition_code), 1, n_caught)) %>%
  mutate(n_kept = ifelse(!is.na(condition_code), 0, n_kept)) %>%
  mutate(n_returned_alive = ifelse(condition_code == "A"|condition_code == "I", 1, 0)) %>%
  mutate(n_returned_dead = ifelse(condition_code == "D", 1, 0)) %>%
  mutate(n_returned_unknown = ifelse(condition_code == "U", 1, 0)) %>%
  # add data source column
  mutate(data_source = "SWFSC 2000s") %>%
  rename(n_retained = n_kept, n_discarded_dead = n_returned_dead, n_discarded_alive = n_returned_alive, n_discarded_unknown = n_returned_unknown) %>%
  mutate(n_discarded_total = n_discarded_alive + n_discarded_dead + n_discarded_unknown) %>%
  select(-trip_id, -spp_code, -comm_name_orig, -sci_name, -mammal_damage_yn,-n_damaged_mammals, -condition_code, -condition, -sex, -tag_yn)

# add species category to merged observer data of 1980s and 2000s

obs_merge <- rbind(obs_1980_pre_join, obs_2000_pre_join)%>%
  merge(species_key, by = "comm_name") %>%
  rename(sp_category = type) %>%
  select(set_num,set_id,comm_name, sp_category, n_caught, n_retained, n_damaged, n_discarded_dead, n_discarded_alive, n_discarded_unknown, n_discarded_total, data_source)

# Merge observer data with trip data from 1980-2000s
total_merge <- merge(obs_merge, trip_merge, by = "set_id") %>%
  # only keep those with exact fishing locations in lat and long
  filter(!is.na(haul_lat_dd))

# Merge observer data with trip data from 1980s-2000s (keep all data)
total_merge_all <- merge(obs_merge, trip_merge, by = "set_id")

#################################################################################################
# variables want to have on x-axis: latitude, Julian day of year, distance from shore and depth #
#################################################################################################

###################################
### calculate distance to shore ###
##################################

# set up projection to utm11
utm11 <- "+proj=utm +zone=11 +datum=NAD83"

# Convert fishing location data to sp
total_xy_sp <- total_merge %>%
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
total_merge_dist <- left_join(total_merge, dist_df, by = "set_id") %>%
  mutate(dist_km = dist_m/1000)

######################
### add Julian day ###
######################

total_merge_final <- total_merge_dist %>%
  mutate(julian_day = lubridate::yday(date))

total_merge_all_final <- total_merge_all %>%
  mutate(julian_day = lubridate::yday(date))

##############################
### Export the final table ###
##############################
write.table(total_merge_final, file = "data/confidential/processed/fig3_merge_obs_and_trip_data.csv", row.names = F, sep = ",")

write.table(total_merge_all_final, file = "data/confidential/processed/fig3_merge_obs_and_trip_data_all.csv", row.names = F, sep = ",")
