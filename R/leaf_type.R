source(here::here('R/common.R'))

# TraitID 43 -- Leaf type
# Data IDs:
#   - 48 -- Leaf type
#   - 2416:2418 -- Leaf type: broad, needle, scale

lt_long <- trydat %>% 
    filter(TraitID == 43) %>% 
    left_join(datanames) %>% 
    collect(n = Inf)

#lt_long %>% 
    #prep_sheet %>% 
    #write_csv('pft_data/leaf_type.csv')

#lookup(1, 1, 'strin', lt_long, 'pft_data/leaf_type.csv', 'leaf_type')

lt_map <- read_csv('pft_data/leaf_type.csv')

lt_proc <- inner_join(lt_long, lt_map)

lt_species <- lt_proc %>% 
    filter(leaf_type != 'n/a') %>% 
    group_by(AccSpeciesID) %>% 
    summarize(leaf_type = most_frequent(leaf_type, c('needle', 'broad')))

count(lt_species, leaf_type, sort = TRUE)

species_phylo <- species %>% 
    select(-N) %>% 
    left_join(tbl(trydb, 'species_phylo')) %>% 
    collect(n = Inf)

lt_missing <- anti_join(species_phylo, lt_species)
lt_missing %>% 
    #filter(PlantGroup != 'Acrogymnospermae') %>% 
    count(Family, sort = TRUE)

needle_families <- c('Pinaceae', 'Cupressaceae')
broad_families <- c('Poaceae', 'Fabaceae', 'Orchidaceae', 'Myrtaceae', 'Rosaceae', 'Cyperaceae',
                    'Rubiaceae', 'Iridaceae', 'Lamiaceae', 'Proteaceae', 'Asteraceae',
                    'Malvaceae', 'Euphorbiaceae')

lt_filled <- lt_missing %>% 
    mutate(leaf_type = case_when(!is.na(.$Family) & .$Family %in% needle_families ~ 'needle',
                                 !is.na(.$Family) & .$Family %in% broad_families ~ 'broad',
                                 TRUE ~ NA_character_)) %>% 
    filter(!is.na(leaf_type)) %>% 
    distinct(AccSpeciesID, leaf_type)

lt_species <- lt_species %>% 
    anti_join(lt_missing) %>% 
    full_join(lt_filled)
    
saveRDS(lt_species, 'processed/pfts/leaf_type.rds')

# Also, may be of interest...
# TraitID 154 -- Leaf shape
# Data IDs:
#   - 18 -- Leaf compoundness
#   - 471 -- Leaf shape

