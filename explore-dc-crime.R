library(tidyverse)
library(sf)
library(leaflet)
library(janitor)
library(tigris)
library(tmap)
library(tidycensus)

dc_crime <- st_read("https://opendata.arcgis.com/datasets/dc3289eab3d2400ea49c154863312434_8.geojson")

dc_crime <- clean_names(dc_crime)

# Leaflet map
leaflet() %>% 
  addProviderTiles(providers$CartoDB) %>% 
  addCircles(data = dc_crime)

# Get DC census tracts, want data too eventually from tidycensus
options(tigris_class = "sf")
dc_tracts <- tracts("DC", cb = T) %>% 
  clean_names()

ggplot(dc_tracts) + 
  geom_sf()

# I'll get an error doing this
# st_join(dc_crime, dc_tracts)

st_crs(dc_crime)
dc_crime_proj <- st_transform(dc_crime, 102285)

st_crs(dc_tracts)
dc_tracts_proj <- st_transform(dc_tracts, 102285) %>% 
  clean_names()

st_crs(dc_crime_proj)
st_crs(dc_tracts_proj)

crime_by_tract <- dc_crime %>% 
  group_by(census_tract) %>% 
  tally() %>% 
  `st_geometry<-`(NULL)

crimes_by_tract <- left_join(dc_tracts_proj, crime_by_tract, by = c("tractce" = "census_tract")) %>% 
  rename(number_crimes = n)

tm_shape(crimes_by_tract) +
  tm_fill("number_crimes", palette = "Reds", legend.hist = T) +
  tm_borders()

# This is a spatially extensive variable
# We want crimes per capita!
dc_pop <- get_decennial(geography = "tract", 
                        variables = c("H0100001"), 
                        state = "DC")

crime_per_1000 <- dc_pop %>% 
  mutate(tractce = str_sub(GEOID, 6, 11)) %>% 
  left_join(crimes_by_tract, ., by = "tractce") %>% 
  mutate(crime_by_capita = number_crimes/value)

tm_shape(filter(crime_per_1000, crime_by_capita < .1)) +
  tm_fill("crime_by_capita", palette = "Reds", legend.hist = T) +
  tm_borders()

v16 <- load_variables(2016, "acs5", cache = TRUE)

filter(crime_per_1000, is.na(crime_by_capita))

# Maybe I want to plot crimes and median income
dc_inc <- get_acs(geography = "tract", variables = c("B19013_001"), state = "DC") %>% 
  clean_names() %>% 
  mutate(census_tract = str_sub(geoid, 6, 11)) %>% 
  select(med_inc = estimate, census_tract)

# Join income to original
med_inc <- left_join(crime_per_1000, dc_inc, by = c("tractce" = "census_tract"))

tmap_mode("view")
tm_shape(med_inc) +
  tm_polygons("med_inc") +
  tm_shape(dc_crime_proj) +
  tm_dots() +
  tm_view(alpha = 1,
          basemaps = "CartoDB.Positron")

ggplot(dc_inc, aes(estimate)) +
  geom_histogram()


