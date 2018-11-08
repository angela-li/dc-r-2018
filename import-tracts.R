options(tigris_class = "sf")

library(tigris)
dc_tracts <- tigris::tracts("DC", cb = T) %>% 
  janitor::clean_names()

saveRDS(dc_tracts, "dc_tracts.Rda")