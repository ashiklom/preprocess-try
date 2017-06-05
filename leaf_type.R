source('common.R')

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
    #write_csv('attribute_maps/leaf_type.csv')

#lookup(1, 1, 'strin', lt_long, 'attribute_maps/leaf_type.csv', 'leaf_type')

lt_map <- read_csv('attribute_maps/leaf_type.csv')

lt_proc <- inner_join(lt_long, lt_map)

lt_species <- lt_proc %>% 
    filter(leaf_type != 'n/a') %>% 
    group_by(AccSpeciesID) %>% 
    summarize(leaf_type = most_frequent(leaf_type, c('needleleaf', 'broadleaf')))

count(lt_species, leaf_type, sort = TRUE)

saveRDS(lt_species, 'attributes/leaf_type.rds')

# Also, may be of interest...
# TraitID 154 -- Leaf shape
# Data IDs:
#   - 18 -- Leaf compoundness
#   - 471 -- Leaf shape

