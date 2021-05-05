# 02-auto-analysis.R

library(sf)
library(tigris)
library(ggplot2)
library(dplyr)
library(tidyr)
library(mapview)
library(rmapshaper)

# Create a data frame containing each TX block group along with key performance 
# measures of auto travel time to the nearest drop location under the one- and 
# multiple-box scenarios.

# Define constants -------------------------------------------------------------
fgdb <- "data/TXTimes_AK.gdb" 

tx_counties <- counties(state = "TX")

tx_bgs <- 
  block_groups(state = "TX") %>%
  st_transform(6579) %>% # https://epsg.io/6579
  # st_simplify(preserveTopology = T, dTolerance = 100)
  ms_simplify()

# Define functions -------------------------------------------------------------
cut_times <- function(times_to_cut) {
  the_cuts <- cut(times_to_cut, breaks = c(0, 10, 20, 30, 40, 50, 60, 120, 180))
}

## Read data -------------------------------------------------------------------

# Travel times from every origin block group to the nearest five drop boxes
# when only one box is available per county
auto_times <- rbind(
  st_drop_geometry(st_read(dsn = fgdb, layer = "ODLinesu1t978")), # Central time
  st_drop_geometry(st_read(dsn = fgdb, layer = "ODLines1jkyfhs"))) # Mountain time

# Travel times from Harris and Travis block groups to all available drop boxes
# when Travis has four and Harris has 12
auto_times_ht <- rbind(
  st_drop_geometry(st_read(dsn = fgdb, layer = "ODLines1u62e74")),
  st_drop_geometry(st_read(dsn = fgdb, layer = "ODLines1jlbzjo")))

# Drop locations
drop_locations_254 <- st_read(dsn = fgdb, layer = "destinations_drop_locations")


## Clean data ------------------------------------------------------------------

auto_times <- auto_times %>%
  separate(Name, into = c("o_geoid", "d_geoid"), sep = " - ", remove = FALSE) %>%
  mutate(o_cty = substr(o_geoid, 3, 5), time_cuts = cut_times(Total_TravelTime)) %>%
  filter(o_cty == d_geoid)

auto_times_ht <- auto_times_ht %>%
  separate(Name, into = c("o_geoid", "d_geoid"), sep = " - ", remove = FALSE) %>%
  mutate(o_cty = substr(o_geoid, 3, 5)) %>%
  group_by(o_geoid, o_cty) %>%
  summarize(auto_time = min(Total_TravelTime))

## Plot results ----------------------------------------------------------------
missing_bgs <- anti_join(tx_bgs, auto_times, by = c("GEOID" = "o_geoid"))

ggplot() + 
  geom_sf(data = missing_bgs, col = "red", fill = "red") + 
  geom_sf(data = tx_counties, col = "black", fill = NA) + 
  geom_sf_text(data = tx_counties, aes(label = COUNTYFP), colour = "black")
  theme_bw()


auto_plot <- inner_join(tx_bgs, auto_times, by = c("GEOID" = "o_geoid"))

ggplot() + 
  geom_sf(data = auto_plot, aes(col = time_cuts, fill = time_cuts)) + 
  geom_sf(data = drop_locations_254, col = "black") + 
  geom_sf(data = tx_counties, col = "black", fill = NA) + 
  scale_fill_viridis_d(direction = -1) + scale_color_viridis_d(direction = -1)

mapview(auto_plot, zcol = "time_cuts")

## Fill in a few missing block groups ------------------------------------------
library(mapsapi)

# key <- YOUR_KEY_HERE
  
missing_bgs <- filter(missing_bgs, COUNTYFP %in% c("201"))
missing_origins <- filter(origins, geoid %in% missing_bgs$GEOID) # need points for origins
missing_drops <- semi_join(drop_off_locations_254, select(st_drop_geometry(missing_bgs), COUNTYFP), by = c("COUNTYFP10" = "COUNTYFP"))

# Create results object for travel 
nrows <- nrow(missing_bgs)

example_route <- mp_directions(
    origin = missing_origins[1, ],
    destination = missing_drops[1, ],
    mode = "driving",
    departure_time = as.POSIXct("2021-05-04 12:00:00 CDT"), # has to be in the future
    alternatives = FALSE,
    key = key,
    quiet = FALSE
  )

route <- mp_get_routes(example_route)

# https://stackoverflow.com/questions/8753531/repeat-rows-of-a-data-frame-n-times
nearest_routes_drive <- do.call("rbind", replicate(nrows, route, simplify = FALSE))

# Calculate all travel times to nearest three dropoff locations
for(i in 1:nrows) {
  # Retrieve directions
  this_route <- mp_directions(
    origin = missing_origins[i, ],
    destination = missing_drops[1, ],
    mode = "driving",
    departure_time = as.POSIXct("2021-05-04 12:00:00 CDT"), # has to be in the future
    alternatives = FALSE,
    key = key,
    quiet = FALSE
  )
  
  nearest_routes_drive[i, ] <- mp_get_routes(this_route)
}

missing_bgs_final <- st_drop_geometry(cbind(missing_bgs, st_drop_geometry(select(nearest_routes_drive, duration_in_traffic_s)))) %>%
  select(o_geoid = GEOID, Total_TravelTime = duration_in_traffic_s) %>%
  mutate(Total_TravelTime = Total_TravelTime / 60)

auto_times <- bind_rows(auto_times, missing_bgs_final)

save(auto_times, file = "output/auto_times_all.RData")
save(auto_times_ht, file = "output/auto_times_ht.RData")
