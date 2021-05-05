# 01-transit-analysis.R

library(tidycensus)
library(tigris)

# allocate RAM memory to Java
options(java.parameters = "-Xmx2G")
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk-11.0.10')
Sys.setenv(TZ = 'America/Chicago')

# 1) build transport network, pointing to the path where OSM and GTFS data are stored
library(r5r)
library(dplyr)
library(sf)
path <- ("data/GTFS_final")
r5r_core <- setup_r5(data_path = path, verbose = FALSE)

# Load origin/destination points

# counties with feeds
transit_counties <- c(
  "Angelina",
  "Bexar", 
  "Brazos",
  "Dallas",
  "Denton",
  "El Paso",
  "Galveston",
  "Harris",
  "Jones",
  "Liberty",
  "Lubbock",
  "Montgomery",
  "Nacogdoches",
  "Nueces",
  "Potter",
  "Runnels",
  "Tarrant",
  "Taylor",
  "Travis")

origins <- 
  read.csv(
    "data/CenPop2010_Mean_BG48.txt",
    colClasses = c("character", "character", "character", "character",
                   "numeric", "numeric", "numeric")) %>%
  mutate(geoid = paste0(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE)) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326, agr = "constant")
  
drop_off_locations_254 <- 
  st_read("data/drop-off locations/drop_locations_254.shp")

# destn_boxes <- inner_join(
#   drop_off_locations_254, 
#   select(st_drop_geometry(origins), COUNTYFP),
#   by = c("COUNTYFP10" = "COUNTYFP"))

# Subregion is county in the code below
drop_off_locations_ht <- 
  rbind(
  mutate(select(st_read("data/drop-off locations/drop_locations_Harris_FINAL.shp"), geometry), COUNTYFP = "201"),
  mutate(select(st_read("data/drop-off locations/drop_locations_Travis_FINAL.shp"), geometry), COUNTYFP = "453"))
  
# Set routing parameters
mode <- c("WALK", "TRANSIT")
max_walk_dist <- 5000   # meters
max_trip_duration <- 180 # minutes
# departure_datetime <- as.POSIXct("10-27-2020 12:00:00", format = "%m-%d-%Y %H:%M:%S")

# 5-minute blocks between 9am and 6pm central (8 and 5 mountain)
set.seed(732)
departure_datetimes <- 
  seq(as.POSIXct("10-27-2020 9:00:00", format = "%m-%d-%Y %H:%M:%S"), 
      as.POSIXct("10-27-2020 17:55:00", format = "%m-%d-%Y %H:%M:%S"), 
      by = "5 min") +
  runif(108, 0, 300)

attr(departure_datetimes, "tzone") <- "" # This shouldn't be necessary 

# Travel time matrix for the entire state with one box - every origin to every box
ttm_all <- 
  do.call(
    rbind,
      lapply(departure_datetimes, 
             function(x) {
               ttm_now <- 
                 travel_time_matrix(
                    r5r_core,
                    origins = select(origins, geoid, geometry),
                    destinations = select(drop_off_locations_254, GEOID10, geometry),
                    mode = mode,
                    departure_datetime = x,
                    max_walk_dist = max_walk_dist,
                    max_trip_duration = max_trip_duration,
                    verbose = FALSE)
               ttm_now$slice <- x
               
               return(ttm_now)
             })
  )

save(ttm_all, file = "output/ttm_all_full.RData")
load(file = "output/ttm_all_full.RData")

# Include only origins with their destination drop box in the same county
# Calculate total trip count
ttm_summary <- ttm_all %>%
  mutate(fromCty = substr(fromId, 1, 5)) %>%
  filter(fromCty == toId) %>%
  group_by(fromId, toId) %>%
  summarize(mean_time = mean(travel_time),
            trips = n(),
            final_mean = ((108 - trips) * 180 + trips * mean_time) / 108)
 
# table(ttm_all$trips) # Most OD pairs have times for all slices
# Set others at max time - 180 for purposes of averaging 

save(ttm_summary, file = "output/ttm_all_summary.RData")

ttm_ht <- 
  do.call(
    rbind,
      lapply(departure_datetimes, 
             function(x) {
               ttm_now <- 
                 travel_time_matrix(
                    r5r_core,
                    origins = select(filter(origins, COUNTYFP %in% c("201", "453")), geoid, geometry),
                    destinations = select(drop_off_locations_ht, COUNTYFP, geometry),
                    mode = mode,
                    departure_datetime = x,
                    max_walk_dist = max_walk_dist,
                    max_trip_duration = max_trip_duration,
                    verbose = FALSE)
               ttm_now$slice <- x
               
               return(ttm_now)
             })
  )

save(ttm_ht, file = "output/ttm_ht_full.RData")

# Include only origins with their destination drop box in the same county
# Calculate total trip count
ttm_ht_summary <- ttm_ht %>%
  mutate(fromCty = substr(fromId, 3, 5)) %>%
  filter(fromCty == toId) %>%
  group_by(fromId, toId, slice) %>%
  summarize(travel_time = min(travel_time)) %>%
  summarize(mean_time = mean(travel_time),
            trips = n(),
            final_mean = ((108 - trips) * 180 + trips * mean_time) / 108)

save(ttm_summary, file = "output/ttm_ht_summary.RData")

stop_r5(r5r_core)
rJava::.jgc(R.gc = TRUE)
