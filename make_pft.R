# Need the following:
#   - [X] Phenology
#   - [X] Growth form
#   - [X] C3 vs C4
#   - [X] Leaf type

library(tidyverse)

#growth_form_ignore <- c('liana_climber', 'cryptophyte')
#growth_form_nonwoody <- c('graminoid', 'forb_herb', 'hemicryptophyte',
                          #'therophyte', 'chamaephyte', 'fern',
                          #'epiphyte', 'geophyte', 'perennial')

species_phylo <- src_sqlite('try.sqlite') %>% tbl('species_phylo') %>% collect()

plant_attrs_raw <- readRDS('attributes/leaf_type.rds') %>% 
    full_join(readRDS('attributes/phenology.rds')) %>% 
    full_join(readRDS('attributes/ps_pathway.rds')) %>% 
    full_join(readRDS('attributes/growth_form.rds')) %>% 
    full_join(readRDS('attributes/climate_zone.rds')) %>% 
    left_join(species_phylo)

plant_attrs_raw %>% distinct(growth_form)

plant_attrs <- plant_attrs_raw %>% 
    mutate(woodiness = case_when(!is.na(.$growth_form) & .$growth_form == 'woody' ~ 'woody',
                                 !is.na(.$growth_form) & .$growth_form != 'woody' ~ 'nonwoody',
                                 TRUE ~ NA_character_))

assign_pft <- function(growth_form, ps_pathway, woodiness, phenology, leaf_type, climate_zone) {
    pft <- NA_character_
    # First try based on attributes
    # `isTRUE` is necessary to handle missing values
    if (isTRUE(growth_form == 'succulent' | ps_pathway == 'CAM')) {
        pft <- 'succulent'
    } else if (isTRUE(ps_pathway == 'C4')) {
        pft <- 'C4'
    } else if (isTRUE(woodiness == 'woody')) {
        if (isTRUE(leaf_type == 'broad')) {
            if (isTRUE(phenology == 'deciduous')) {
                if (isTRUE(climate_zone == 'tropical')) {
                    pft <- 'tropical_deciduous_broadleaf'
                } else if (isTRUE(climate_zone %in% c('temperate', 'boreal'))) {
                    pft <- 'temperate_deciduous_broadleaf'
                }
            } else if (isTRUE(phenology == 'evergreen')) {
                if (isTRUE(climate_zone == 'tropical')) {
                    pft <- 'tropical_evergreen_broadleaf'
                } else if (isTRUE(climate_zone %in% c('temperate', 'boreal'))) {
                    pft <- 'temperate_evergreen_broadleaf'
                }
            }
        } else if (isTRUE(leaf_type == 'needle')) {
            if (isTRUE(phenology == 'deciduous')) {
                pft <- 'deciduous_conifer'
            } else {
                pft <- 'evergreen_conifer'
            }
        }
    } else if (isTRUE(woodiness == 'nonwoody')) {
        if (isTRUE(growth_form == 'graminoid')) {
            pft <- 'C3_graminoid'
        } else if (isTRUE(growth_form == 'forb')) {
            pft <- 'C3_forb'
        }
    }
    return(pft)
}

pfts <- plant_attrs %>% 
    rowwise() %>% 
    mutate(pft = assign_pft(growth_form, ps_pathway, woodiness, phenology, leaf_type, climate_zone)) %>% 
    'class<-'(c('tbl_df', 'data.frame'))

saveRDS(pfts, 'all_pfts.rds')

distinct_pfts <- pfts %>% 
    filter(!is.na(pft)) %>% 
    distinct(AccSpeciesID, pft)

traits_fill <- readRDS('trait_data.rds')

traits_pfts <- left_join(traits_fill, distinct_pfts)

saveRDS(traits_pfts, file = 'traits_pfts.rds')

traits_analysis <- traits_pfts %>% 
    filter(!is.na(pft)) %>% 
    select(ObservationID, AccSpeciesID, pft, which(sapply(., is_double)), 
           -Latitude, -Longitude, -Temperature_measurement)

saveRDS(traits_analysis, file = 'traits_analysis.rds')
############################################################

#plant_attrs %>% count(phenology, sort = TRUE)

#plant_attrs %>% filter(is.na(growth_form))

#plant_attrs %>% filter(phenology == 'deciduous', leaf_type == 'needle') %>% count(growth_form)

