library(tidyverse)
library(sp)

trait_data <- readRDS('traits/trait_data.rds')

#coords <- distinct(trait_data, Longitude, Latitude) %>% 
    #filter(!is.na(Latitude), !is.na(Longitude))

#library(sp)
#library(rgdal)

## Same projection as WorldClim
#proj_string <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#coords_proj <- SpatialPoints(coords, CRS(proj_string))

#saveRDS(coords_proj, 'latlon_coords.rds')

coords_proj <- readRDS('climate/latlon_coords.rds')
coords_df <- as_data_frame(coords_proj)
temp_values <- readRDS('climate/try_temperature_values.rds')

temp_data <- mutate(coords_df, AMT = temp_values)

traits_with_climate <- left_join(trait_data, temp_data)

traits_with_climate %>% count(is.na(AMT))

saveRDS(traits_with_climate, 'traits/traits_with_climate.rds')
