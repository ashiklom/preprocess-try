# Need the following:
#   - [X] Phenology
#   - [X] Growth form
#   - [X] C3 vs C4
#   - [X] Leaf type

# PFTs:
#   - Growth form: Liana_climber (ignore)
#   - Growth form: Hydrophyte (ignore)
#   - Growth form: Succulent
#   - PS pathway == C4
#       * Woodiness: woody -- C4_tree
#       * Woodiness: non-woody -- C4_grass
#   - PS pathway != C4
#       * Growth form: Tree, shrub
#           - Leaf type: Broadleaf
#               * Phenology: Deciduous
#                   - By biome
#               * Phenology: Evergreen
#                   - By biome?
#           - Leaf type: Needleleaf
#       * Growth form: Forb/herb
#       * Growth form: Graminoid (C3 grass)

library(tidyverse)

growth_form_ignore <- c('liana_climber', 'cryptophyte')
growth_form_nonwoody <- c('graminoid', 'forb_herb', 'hemicryptophyte',
                          'therophyte', 'chamaephyte', 'fern',
                          'epiphyte', 'geophyte', 'perennial')

species_phylo <- src_sqlite('try.sqlite') %>% tbl('species_phylo') %>% collect()

plant_attrs <- read_csv('attributes/leaf_type.csv') %>% 
    full_join(read_csv('attributes/phenology.csv')) %>% 
    full_join(read_csv('attributes/ps_pathway.csv')) %>% 
    full_join(read_csv('attributes/growth_form.csv')) %>% 
    left_join(species_phylo) %>% 
    filter(!growth_form %in% growth_form_ignore) %>% 
    mutate(woodiness = case_when(.$growth_form == 'woody' ~ 'woody',
                                 .$growth_form %in% growth_form_nonwoody ~ 'nonwoody',
                                 TRUE ~ NA_character_))

pfts <- character(nrow(plant_attrs))

assign_pft <- function(growth_form, ps_pathway, woodiness, phenology, leaf_type) {
    pft <- NA_character_
    # First try based on attributes
    # `isTRUE` is necessary to handle missing values
    if (isTRUE(growth_form == 'succulent' | ps_pathway == 'CAM')) {
        pft <- 'succulent'
    } else if (isTRUE(ps_pathway == 'C4')) {
        if (isTRUE(woodiness == 'woody')) {
            pft <- 'C4_tree'
        } else {
            pft <- 'C4_grass'
        }
    } else if (isTRUE(woodiness == 'woody')) {
        if (isTRUE(leaf_type == 'broad')) {
            if (isTRUE(phenology == 'deciduous')) {
                pft <- 'deciduous_broadleaf'
            } else if (isTRUE(phenology == 'evergreen')) {
                pft <- 'evergreen_broadleaf'
            }
        } else if (isTRUE(leaf_type == 'needle')) {
            if (isTRUE(phenology == 'deciduous')) {
                pft <- 'deciduous_conifer'
            } else {
                pft <- 'evergreen_conifer'
            }
        }
    } else if (isTRUE(woodiness == 'nonwoody')) {
        pft <- 'C3_herb'
    }
    return(pft)
}

pfts <- plant_attrs %>% 
    rowwise() %>% 
    mutate(pft = assign_pft(growth_form, ps_pathway, woodiness, phenology, leaf_type)) %>% 
    'class<-'(c('tbl_df', 'data.frame'))

saveRDS(pfts, 'all_pfts.rds')

pfts %>% 
    filter(!is.na(pft)) %>% 
    distinct(AccSpeciesID, pft) %>% 
    write_csv('try_pfts.csv')

############################################################

#plant_attrs %>% count(phenology, sort = TRUE)

#plant_attrs %>% filter(is.na(growth_form))

#plant_attrs %>% filter(phenology == 'deciduous', leaf_type == 'needle') %>% count(growth_form)

