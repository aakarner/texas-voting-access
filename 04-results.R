# 04-results.R
## Summarize demographics for the one-drop and all-drop scenarios

library(dplyr)
library(sf)
library(ggplot2)

## Load data -------------------------------------------------------------------

# PT travel times from every bg to drop-off locations with only one in each county
load("output/ttm_all_summary.RData")

# PT travel times from Harris and Travis bgs to full complement of drop-off locations
load("output/ttm_ht_summary.RData")

# Auto travel times from every bg to drop-off locations with only one in each county
load("output/auto_times_all.RData")

# Auto travel times from Harris and Travis bgs to full complement of drop-off locations
load("output/auto_times_ht.RData")

# Build data in long format

# Pre-process Harris and Travis transit data so that we have a consistent set of block groups included
# Assign max travel time of 180 mins
loses_transit <- anti_join(ttm_ht_summary, filter(ttm_summary, toId %in% c("48201", "48453")), by = "fromId") %>%
  mutate(final_mean = 180)

all_modes <-
  rbind(
    mutate(select(ttm_summary, origin = fromId, time = final_mean), mode = "transit", scenario = "one drop-off"),
    mutate(select(loses_transit, origin = fromId, time = final_mean), mode = "transit", scenario = "one drop-off"),
    mutate(select(ttm_ht_summary, origin = fromId, time = final_mean), mode = "transit", scenario = "all drop-offs"),
    mutate(select(auto_times, origin = o_geoid, time = Total_TravelTime), mode = "auto", scenario = "one drop-off"),
    mutate(select(auto_times_ht, origin = o_geoid, time = auto_time), mode = "auto", scenario = "all drop-offs")
  ) %>%
  mutate(time_cuts = cut_times(time))

drop_off_locations_harris <- 
  st_read("data/drop-off locations/drop_locations_Harris_FINAL.shp") %>%
  mutate(scenario = "all drop-offs")
drop_off_locations_harris[13, ] <- drop_off_locations_harris[11, ]
drop_off_locations_harris[13, "scenario"] <- "one drop-off"
  
drop_off_locations_travis <- 
  st_read("data/drop-off locations/drop_locations_Travis_FINAL.shp") %>%
  mutate(scenario = "all drop-offs")
drop_off_locations_travis[5, ] <- drop_off_locations_travis[4, ]
drop_off_locations_travis[5, "scenario"] <- "one drop-off"

all_modes_bgs <- 
  voters_bgs_plot %>%
  inner_join(all_modes, by = c("GEOID" = "origin"))
  
# Voters living within walking or walk + transit distance of the drop off location
# tx_summary_transit <- 
#   st_drop_geometry(voters_bgs_times) %>%
#   filter(!is.na(transit_time)) %>%
#   summarize(tot = sum(cvap_est_total),
#             tot_Black = sum(cvap_est_Black),
#             tot_White = sum(cvap_est_White),
#             tot_Latinx = sum(cvap_est_Latinx),
#             tot_Asian = sum(cvap_est_Asian),
#             tot_zvh = sum(zv_voters))
# 
# tx_summary_total <- 
#   st_drop_geometry(voters_bgs_times) %>%
#   summarize(tot = sum(cvap_est_total),
#             tot_Black = sum(cvap_est_Black),
#             tot_White = sum(cvap_est_White),
#             tot_Latinx = sum(cvap_est_Latinx),
#             tot_Asian = sum(cvap_est_Asian),
#             tot_zvh = sum(zv_voters))
# 
# county_summary <- 
#   voters_bgs_times %>%
#   filter(COUNTYFP %in% transit_counties) %>%
#   group_by(COUNTYFP) %>%
#   summarize(bg_transit_time = sum(transit_time * zv_voters, na.rm = TRUE) / sum(zv_voters, na.rm = TRUE),
#             bg_auto_time = sum(auto_time * cvap_est_total - zv_voters) / sum(cvap_est_total - zv_voters)) %>%
#   inner_join(select(st_drop_geometry(tx_counties), COUNTYFP, NAME))

## Plot results ----------------------------------------------------------------

scale_params <- tibble::tibble(
  scenario = c("one drop-off"),
  mode = c("auto"),
  width_hint = 0.25,
  location = c("bl"),
  unit_category = c("imperial")
)

na_params <- tibble::tibble(
  scenario = c("all drop-offs"),
  mode = c("transit"),
  location = c("tr")
)

# Harris County results
ggplot() + 
  geom_sf(data = filter(all_modes_bgs, COUNTYFP == "201"), aes(col = time_cuts, fill = time_cuts)) + 
  geom_sf(data = tx_counties, col = grey(0.7), fill = "transparent") + 
  geom_sf(data = drop_off_locations_harris, aes(shape = 21), col = "gray", fill = "blue", size = 1.5) + 
  facet_grid(scenario ~ mode, switch = "y") + 
  xlab(NULL) + ylab(NULL) + 
  annotation_scale(aes(location = location, unit_category = unit_category), data = scale_params) + 
  annotation_north_arrow(aes(location = location), data = na_params, style = north_arrow_minimal) + 
  # coord_sf(xlim = c(1846977.57, 2040794.09), ylim = c(6263897.91, 6384129.30), default_crs = NULL, expand = FALSE) +
  coord_sf(xlim = c(-96, -94.85), ylim = c(29.5, 30.2), expand = TRUE) +
  scale_fill_viridis_d(name = "travel time (min) to nearest\ndrop-off location", option = "inferno", direction = -1) + 
  scale_color_viridis_d("travel time (min) to nearest\ndrop-off location", option = "inferno", direction = -1) + 
  scale_continuous_identity(name = NULL, aesthetics = "shape",labels = "drop-off location", guide = "legend") + 
  theme_bw() + 
  theme(axis.ticks = element_blank(), axis.title = element_blank(), 
        panel.background = element_blank(), axis.text = element_blank(),
        panel.grid = element_blank(), panel.spacing = unit(0, 
          "lines"), plot.background = element_blank())

ggsave("output/Fig1_HarrisTimes.png", bg = "white", width = 8, height = 4)

# Travis County results
ggplot() + 
  geom_sf(data = filter(all_modes_bgs, COUNTYFP == "453"), aes(col = time_cuts, fill = time_cuts)) + 
  geom_sf(data = tx_counties, col = "black", fill = "transparent") + 
  geom_sf(data = drop_off_locations_travis, aes(shape = 21), col = "gray", fill = "blue") + 
  facet_grid(scenario ~ mode, switch = "y") + 
  xlab(NULL) + ylab(NULL) + 
  coord_sf(xlim = c(-98.2, -97.35), ylim = c(30.0, 30.65), expand = TRUE) +
  scale_fill_viridis_d(name = "travel time to nearest\ndrop-off location (min)", option = "inferno", direction = -1) + 
  scale_color_viridis_d("travel time to nearest\ndrop-off location (min)", option = "inferno", direction = -1) + 
  scale_continuous_identity(name = NULL, aesthetics = "shape",labels = "drop-off location", guide = "legend") + 
  theme_bw() + 
  theme(axis.ticks = element_blank(), axis.title = element_blank(), 
        panel.background = element_blank(), axis.text = element_blank(),
        panel.grid = element_blank(), panel.spacing = unit(0, 
          "lines"), plot.background = element_blank())

ggsave("output/Fig2_TravisTimes.png", bg = "white", width = 8, height = 4)

# Weighted summaries for all counties/all modes/all groups
transit_counties <- c("005", "029", "041", "113", "121", "141", "167", "201",
                     "291", "303", "339", "347", "355", "375", "439", "441", "453")

ht <- c("201", "453")

# Need data in a wider format to weight households by auto ownership
all_modes_wide <-
  rbind(
    left_join(
      mutate(select(auto_times, origin = o_geoid, auto_time = Total_TravelTime), scenario = "one drop-off"),
      mutate(rbind(
        select(ttm_summary, origin = fromId, transit_time = final_mean),
        select(loses_transit, origin = fromId, transit_time = final_mean)))),
    left_join(
      mutate(select(auto_times_ht, origin = o_geoid, auto_time = auto_time), scenario = "all drop-offs"),
      mutate(select(ttm_ht_summary, origin = fromId, transit_time = final_mean)))
  ) %>%
  right_join(voters_bgs, by = c("origin" = "geoid_bg"))

# Blended time  
# results_summary <- 
#   all_modes_wide %>%
# #   mutate(transit_county = ifelse(county %in% transit_counties, 1, 0)) %>%
#   group_by(county, scenario) %>%
#   summarize(time_transit = sum(transit_time * cvap_est_total * zvh_share, na.rm = TRUE) / sum(cvap_est_total * zvh_share),
#             time_auto = sum(auto_time * cvap_est_total * (1 - zvh_share), na.rm = TRUE) / sum(cvap_est_total * (1 - zvh_share)),
#             time_black = (sum(transit_time * cvap_est_Black * zvh_share, na.rm = TRUE) + sum(auto_time * cvap_est_Black * (1 - zvh_share), na.rm = TRUE)) / sum(cvap_est_Black),
#             time_white = (sum(transit_time * cvap_est_White * zvh_share, na.rm = TRUE) + sum(auto_time * cvap_est_White * (1 - zvh_share), na.rm = TRUE)) / sum(cvap_est_White),
#             time_latinx = (sum(transit_time * cvap_est_Latinx * zvh_share, na.rm = TRUE) + sum(auto_time * cvap_est_Latinx * (1 - zvh_share), na.rm = TRUE)) / sum(cvap_est_Latinx),
#             time_asian = (sum(transit_time * cvap_est_Asian * zvh_share, na.rm = TRUE) + sum(auto_time * cvap_est_Asian * (1 - zvh_share), na.rm = TRUE)) / sum(cvap_est_Asian)
#             ) %>%
#   pivot_longer(time_transit:time_asian)
# 

# Just transit
results_summary <- 
  all_modes_wide %>%
#   mutate(transit_county = ifelse(county %in% transit_counties, 1, 0)) %>%
  group_by(county, scenario) %>%
  summarize(time_transit = sum(transit_time * cvap_est_total * zvh_share, na.rm = TRUE) / sum(cvap_est_total * zvh_share),
            time_auto = sum(auto_time * cvap_est_total * (1 - zvh_share), na.rm = TRUE) / sum(cvap_est_total * (1 - zvh_share)),
            time_black = sum(transit_time * cvap_est_Black * zvh_share, na.rm = TRUE) / sum(cvap_est_Black * zvh_share),
            time_white = sum(transit_time * cvap_est_White * zvh_share, na.rm = TRUE) / sum(cvap_est_White * zvh_share),
            time_latinx = sum(transit_time * cvap_est_Latinx * zvh_share, na.rm = TRUE) / sum(cvap_est_Latinx * zvh_share),
            time_asian = sum(transit_time * cvap_est_Asian * zvh_share, na.rm = TRUE) / sum(cvap_est_Asian * zvh_share)
            ) %>%
  pivot_longer(time_transit:time_asian) %>%
  mutate(name = factor(name, labels = c("Asian", "auto\n(voters with vehicles available)", "Black", "Latinx", "transit\n(voters with no vehicles available)", "White")))

filter(results_summary, county == "201")

# How many zero-vehicle voters in Harris in locations where public transit is not available?
all_modes_wide %>%
  group_by(county, scenario) %>%
  filter(county == "201" & is.na(transit_time)) %>%
  summarize(total_zvs = sum(cvap_est_total * zvh_share))

all_modes_wide %>%
  group_by(county, scenario) %>%
  filter(county == "201") %>%
  summarize(total_zvs = sum(cvap_est_total * zvh_share))

# Threshold analysis - how many lose 60-minute access? 
all_modes_wide %>%
  mutate(threshold = ifelse(transit_time <= 60, 1, 0)) %>%
  group_by(county, scenario, threshold) %>%
  filter(county == "201") %>%
  summarize(total_thresh = sum(cvap_est_total * zvh_share))

# Mean across the state
ht_results <- 
  all_modes_wide %>%
  filter(county == "201" | county == "453")
  
rest_of_state <-
  all_modes_wide %>%
  filter(county != "201" & county != "453")

# statewide_summary_all <- 
#   rbind(rest_of_state,
#         filter(ht_results, scenario == "all drop-offs")) %>%
# #   mutate(transit_county = ifelse(county %in% transit_counties, 1, 0)) %>%
#   summarize(time_transit = sum(transit_time * cvap_est_total * zvh_share, na.rm = TRUE) / sum(cvap_est_total * zvh_share),
#             time_auto = sum(auto_time * cvap_est_total * (1 - zvh_share), na.rm = TRUE) / sum(cvap_est_total * (1 - zvh_share)),
#             time_black_t = sum(transit_time * cvap_est_Black * zvh_share, na.rm = TRUE) / sum(cvap_est_Black * zvh_share),
#             time_black_a = sum(auto_time * cvap_est_Black * (1 - zvh_share), na.rm = TRUE) / sum(cvap_est_Black * (1 - zvh_share)),
#             time_white_t = sum(transit_time * cvap_est_White * zvh_share, na.rm = TRUE) / sum(cvap_est_White * zvh_share),
#             time_white_a = sum(auto_time * cvap_est_White * (1 - zvh_share), na.rm = TRUE) / sum(cvap_est_White * (1 - zvh_share)),
#             time_latinx_t = sum(transit_time * cvap_est_Latinx * zvh_share, na.rm = TRUE) / sum(cvap_est_Latinx * zvh_share),
#             time_latinx_a = sum(auto_time * cvap_est_Latinx * (1 - zvh_share), na.rm = TRUE) / sum(cvap_est_Latinx * (1 - zvh_share)),
#             time_asian_t = sum(transit_time * cvap_est_Asian * zvh_share, na.rm = TRUE) / sum(cvap_est_Asian * zvh_share),
#             time_asian_a = sum(auto_time * cvap_est_Asian * (1 - zvh_share), na.rm = TRUE) / sum(cvap_est_Asian * (1 - zvh_share)),
#             n = n()
#             ) %>%
#   pivot_longer(time_transit:time_asian_a)

statewide_summary_all <- 
  rbind(rest_of_state,
        filter(ht_results, scenario == "all drop-offs")) %>%
#   mutate(transit_county = ifelse(county %in% transit_counties, 1, 0)) %>%
  summarize(time_transit = sum(transit_time * cvap_est_total * zvh_share, na.rm = TRUE) / sum(cvap_est_total * zvh_share),
            time_auto = sum(auto_time * cvap_est_total * (1 - zvh_share), na.rm = TRUE) / sum(cvap_est_total * (1 - zvh_share)),
            time_black = sum(auto_time * cvap_est_Black * (1 - zvh_share), na.rm = TRUE) / sum(cvap_est_Black * (1 - zvh_share)),
            time_white = sum(auto_time * cvap_est_White * (1 - zvh_share), na.rm = TRUE) / sum(cvap_est_White * (1 - zvh_share)),
            time_latinx = sum(auto_time * cvap_est_Latinx * (1 - zvh_share), na.rm = TRUE) / sum(cvap_est_Latinx * (1 - zvh_share)),
            time_asian = sum(auto_time * cvap_est_Asian * (1 - zvh_share), na.rm = TRUE) / sum(cvap_est_Asian * (1 - zvh_share)),
            n = n()
            ) %>%
  pivot_longer(time_transit:time_asian)

# statewide_summary_one <- 
#   rbind(rest_of_state,
#         filter(ht_results, scenario == "one drop-off")) %>%
# #   mutate(transit_county = ifelse(county %in% transit_counties, 1, 0)) %>%
#   summarize(time_transit = sum(transit_time * cvap_est_total * zvh_share, na.rm = TRUE) / sum(cvap_est_total * zvh_share),
#             time_auto = sum(auto_time * cvap_est_total * (1 - zvh_share), na.rm = TRUE) / sum(cvap_est_total * (1 - zvh_share)),
#             time_black_t = sum(transit_time * cvap_est_Black * zvh_share, na.rm = TRUE) / sum(cvap_est_Black * zvh_share),
#             time_black_a = sum(auto_time * cvap_est_Black * (1 - zvh_share), na.rm = TRUE) / sum(cvap_est_Black * (1 - zvh_share)),
#             time_white_t = sum(transit_time * cvap_est_White * zvh_share, na.rm = TRUE) / sum(cvap_est_White * zvh_share),
#             time_white_a = sum(auto_time * cvap_est_White * (1 - zvh_share), na.rm = TRUE) / sum(cvap_est_White * (1 - zvh_share)),
#             time_latinx_t = sum(transit_time * cvap_est_Latinx * zvh_share, na.rm = TRUE) / sum(cvap_est_Latinx * zvh_share),
#             time_latinx_a = sum(auto_time * cvap_est_Latinx * (1 - zvh_share), na.rm = TRUE) / sum(cvap_est_Latinx * (1 - zvh_share)),
#             time_asian_t = sum(transit_time * cvap_est_Asian * zvh_share, na.rm = TRUE) / sum(cvap_est_Asian * zvh_share),
#             time_asian_a = sum(auto_time * cvap_est_Asian * (1 - zvh_share), na.rm = TRUE) / sum(cvap_est_Asian * (1 - zvh_share)),
#             n = n()
#             ) %>%
#   pivot_longer(time_transit:time_asian_a)

statewide_summary_one <- 
  rbind(rest_of_state,
        filter(ht_results, scenario == "one drop-off")) %>%
#   mutate(transit_county = ifelse(county %in% transit_counties, 1, 0)) %>%
  summarize(time_transit = sum(transit_time * cvap_est_total * zvh_share, na.rm = TRUE) / sum(cvap_est_total * zvh_share),
            time_auto = sum(auto_time * cvap_est_total * (1 - zvh_share), na.rm = TRUE) / sum(cvap_est_total * (1 - zvh_share)),
            time_black = sum(auto_time * cvap_est_Black * (1 - zvh_share), na.rm = TRUE) / sum(cvap_est_Black * (1 - zvh_share)),
            time_white = sum(auto_time * cvap_est_White * (1 - zvh_share), na.rm = TRUE) / sum(cvap_est_White * (1 - zvh_share)),
            time_latinx = sum(auto_time * cvap_est_Latinx * (1 - zvh_share), na.rm = TRUE) / sum(cvap_est_Latinx * (1 - zvh_share)),
            time_asian = sum(auto_time * cvap_est_Asian * (1 - zvh_share), na.rm = TRUE) / sum(cvap_est_Asian * (1 - zvh_share)),
            n = n()
            ) %>%
  pivot_longer(time_transit:time_asian)