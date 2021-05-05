# 03-demographic-analysis.R

library(dplyr)
library(tidyr)
library(tidycensus)
library(sf)
library(tigris)
library(ggplot2)
library(ggspatial)
library(ggthemes)
library(classInt)
library(tmap)

## Spatial data import ---------------------------------------------------------

# Census TIGER line/shapefiles
tx <- states(cb = TRUE) %>% filter(GEOID == 48) %>%
  st_transform(3082)

tx_bgrps <- block_groups(state = "TX", year = 2018) %>% st_transform(3082)
# tx_tracts <- tracts(state = "TX", year = 2018)
tx_counties <- counties(state = "TX") %>% st_transform(3082)

harris_bgs <- filter(tx_bgrps, COUNTYFP == 201)
travis_bgs <- filter(tx_bgrps, COUNTYFP == 453)

harris_water <- st_read("data/harris_water.shp") # From H-GAC
harris_roads <- roads(state = "TX", county = 201)

harris_county <- filter(tx_counties, COUNTYFP == 201)
travis_county <- filter(tx_counties, COUNTYFP == 453)
  
# tract_centers <- 
#   read.csv("data/CenPop2010_Mean_TR48.txt", 
#            colClasses = c(rep("character", 3), rep("numeric", 3))) %>%
#   mutate(geoid = paste0(STATEFP, COUNTYFP, TRACTCE)) %>%
#   filter(COUNTYFP == 201) %>%
#   select(geoid, LATITUDE, LONGITUDE) %>%
#   st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4269) %>%
#   st_transform(4326) # WGS84

## Demographic data import -----------------------------------------------------

# The US census bureau has a special tabulation of citizen voting age population
# by race/ethnicity
voting_demogs <- 
  read.csv("data/CVAP_2014-2018_ACS_csv_files/BlockGr.csv") %>%
  separate(geoid, 
           into = c("US", "state", "county", "tract", "bg"), 
           sep = c(7, 9, 12, 18, 19), 
           remove = FALSE) %>%
  filter(state == "48") %>%
  mutate(lnnumber = 
           factor(lnnumber,
                  labels = c("total",
                             "total_nhl",
                             "AIAN",
                             "Asian",
                             "Black",
                             "NHPI",
                             "White",
                             "AIAN_white",
                             "Asian_white",
                             "Black_white",
                             "AIAN_black",
                             "TwoPlusOther",
                             "Latinx"))) %>%
  pivot_wider(id_cols = c(geoname, state, county, tract, bg),
              names_from = lnnumber, 
              values_from = c(cvap_est, cvap_moe)) %>% # cvap is citizen voting age population
  mutate(cvap_est_other = cvap_est_AIAN_white + cvap_est_Asian_white + 
                          cvap_est_Black_white + cvap_est_AIAN_black + 
                          cvap_est_TwoPlusOther,
         cvap_moe_other = (cvap_moe_AIAN_white^2 + cvap_moe_Asian_white^2 + 
                           cvap_moe_Black_white^2 + cvap_moe_AIAN_black^2 + 
                          cvap_moe_TwoPlusOther^2)^0.5,
         geoid_bg = paste0(state, county, tract, bg),
         geoid_tract = paste0(state, county, tract))

# Households by block group
# hhs_bgs <- 
#   get_acs("block group",
#           state = "TX",
#           variables = "B25001_001",
#           geometry = FALSE)

# Collect ACS data on auto ownership
# v18 <- load_variables(2018, "acs5", cache = TRUE)
# Table B08122 contains counts of commute mode by poverty status
# 014 - transit and below 100% poverty
# 015 - transit and between 100-150% poverty
# transit_pov <- 
#   get_acs("tract", 
#           state = "TX", 
#           county = 201,
#           variables = c("B08122_014", "B08122_015"),
#           summary_var = "B08122_001",
#           geometry = FALSE) %>%
#   pivot_wider(names_from = variable, values_from = c("estimate", "moe")) %>%
#   mutate(transit_pov_est = estimate_B08122_014 + estimate_B08122_015,
#          transit_pov_moe = (moe_B08122_014^2 + moe_B08122_015^2)^0.5,
#          transit_pov_shr = transit_pov_est / summary_est)

# ZVH not available for block groups
zvh_tract <- 
  get_acs("tract",
          state = "TX",
          variables = "B08201_002", # Zero-vehicle households
          summary_var = "B08201_001", # Total households
          geometry = FALSE) %>%
  transmute(geoid_tract = GEOID,
            zvh = estimate,
            zvh_moe = moe)

# Share of ZVH by county - checks out in comparison to SocialExplorer
# zvh_cnty <- 
#   zvh_tract %>%
#   mutate(cnty = substr(GEOID, 3, 5)) %>%
#   group_by(cnty) %>%
#   summarize(zvh_total = sum(estimate),
#             hh_total = sum(summary_est),
#             zvh_share = zvh_total / hh_total)

hhs_tract <-
  get_acs("tract",
          state = "TX",
          variables = "B11001_001",
          geometry = FALSE)

# Apply the tract-level share of zvhs to the bg-level vote counts
voters_bgs <-
  select(voting_demogs,
         county,
         geoid_tract, 
         geoid_bg,
         cvap_est_total, cvap_moe_total,
         cvap_est_Black, cvap_moe_Black,
         cvap_est_White, cvap_moe_White,
         cvap_est_Latinx, cvap_moe_Latinx,
         cvap_est_Asian, cvap_moe_Asian) %>%
  inner_join(zvh_tract) %>%
  inner_join(
    select(hhs_tract, geoid_tract = GEOID, tothhs = estimate, tothhs_moe = moe)) %>%
  mutate(zvh_share = ifelse(tothhs > 0, zvh / tothhs, 0), 
         zv_voters = cvap_est_total * zvh_share,
         zv_voters_share = 
           ifelse(tothhs > 0 & cvap_est_total, zv_voters / cvap_est_total, 0))

summary(voters_bgs$zv_voters_share)

voters_bgs_plot <- inner_join(tx_bgrps, voters_bgs, by = c("GEOID" = "geoid_bg"))

# Collect ACS data on poverty
# poverty <- 
#   get_acs("tract", 
#           state = "TX", 
#           county = 201,
#           variables = c("B17001_002"),
#           summary_var = "B17001_001",
#           geometry = FALSE)

## Demographic summaries -------------------------------------------------------

ggplot(filter(voters_bgs_plot, COUNTYFP == "201")) + 
  geom_sf(aes(col = zv_voters, fill = zv_voters)) + 
  scale_color_viridis_c() + scale_fill_viridis_c() + 
  theme_map() + 
  annotation_scale(location = "tl", unit_category = "imperial") +
  annotation_north_arrow(location = "br", which_north = "true", style = north_arrow_minimal)

# County-level summaries
voters_ctys <- 
  voters_bgs %>%
  group_by(county) %>%
  summarize(zv_voters = sum(zv_voters),
            total_voters = sum(cvap_est_total),
            zv_share = zv_voters / total_voters)

voters_ctys_plot <- inner_join(tx_counties, voters_ctys, by = c("COUNTYFP" = "county"))

tm_shape(voters_ctys_plot) + 
  tm_polygons("zv_voters", 
              style="jenks", 
              title="voters in\nzero-vehicle households")

ggplot(voters_ctys_plot) + 
  geom_sf(aes(col = zv_share, fill = zv_share)) + 
  scale_color_viridis_c() + scale_fill_viridis_c() + 
  theme_map() + 
  annotation_scale(location = "tl", unit_category = "imperial") +
  annotation_north_arrow(location = "br", which_north = "true", style = north_arrow_minimal)


plot_zvh <- inner_join(tx_counties, zvh_cnty, by = c("COUNTYFP" = "cnty"))

ggplot() + geom_sf(data = plot_zvh, aes(col = zvh_share, fill = zvh_share)) + 
  scale_color_viridis_c() + scale_fill_viridis_c()

