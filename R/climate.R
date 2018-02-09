library(tidyverse)

trait_data <- readRDS("processed/traits/trait_data.rds")
lat_lon_data <- readRDS("processed/pfts/lat_lon.rds")
temp_data <- readRDS("pft_data/lat_lon_AMT.rds")

traits_with_climate <- trait_data %>%
  left_join(lat_lon_data) %>%
  left_join(temp_data)
traits_with_climate %>% count(is.na(AMT))
saveRDS(traits_with_climate, "processed/traits/traits_with_climate.rds")
