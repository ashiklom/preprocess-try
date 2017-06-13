library(tidyverse)
library(ncdf4)
library(riri)

trait_data <- readRDS('trait_data.rds')

coords <- distinct(trait_data, Longitude, Latitude) %>% 
    filter(!is.na(Latitude), !is.na(Longitude))

#library(sp)
#library(rgdal)

## Same projection as WorldClim
#proj_string <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#coords_proj <- SpatialPoints(coords, CRS(proj_string))

#saveRDS(coords_proj, 'latlon_coords.rds')

temp_values <- readRDS('try_temperature_values.rds')

temp_data <- mutate(coords, AMT = temp_values)

traits_with_climate <- left_join(trait_data, temp_data)

saveRDS(traits_with_climate, 'traits_with_climate.rds')
