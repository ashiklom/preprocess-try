library(tidyverse)
library(sp)

trait_data <- readRDS("traits/trait_data.rds")
temp_data <- readRDS("pft_data/lat_lon_AMT.rds")

traits_with_climate <- left_join(trait_data, temp_data)
traits_with_climate %>% count(is.na(AMT))
saveRDS(traits_with_climate, "traits/traits_with_climate.rds")
