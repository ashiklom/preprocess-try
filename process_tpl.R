library(tidyverse)
#species_str <- species[['AccSpeciesName']] %>% 
    #stringi::stri_trans_general('latin-ascii')

## Data source 3 -- ITIS
#sp_breaks <- ceiling(seq_along(species_str) / 300)
#z <- gnr_resolve(species_str, best_match_only = TRUE, with_context = TRUE, preferred_data_sources = 3)

#writeLines(species_str, 'all_species')

#library(taxize)

#tpl_get('tpl/')

tpl_list <- list.files('pfts_species/tpl', full.names = TRUE)

message('Reading ThePlantList data...')
tpl_dat <- tpl_list %>% 
    map(data.table::fread, header = TRUE) %>% 
    data.table::rbindlist(fill = TRUE) %>% 
    as_data_frame()

message('Forming species names from ThePlantList data...')
tpl_proc <- tpl_dat %>% 
    mutate(AccSpeciesName = paste(Genus, Species)) %>% 
    distinct(AccSpeciesName, Family, Genus, Species)

#tpl_proc %>% count(AccSpeciesName) %>% filter(n > 1)

message('Counting TPL families...')
tpl_fam <- count(tpl_proc, Family, sort = TRUE)

#tpl_proc %>% glimpse()

#tpl_proc %>% 
    #filter(AccSpeciesName == 'Anacampseros affinis') %>% 
    #glimpse()

# Remove duplicate families by selecting the more common family
message('Removing duplicate family IDs...')
tpl_dupsp <- tpl_proc %>% count(AccSpeciesName) %>% filter(n > 1)
tpl_dupfam <- tpl_proc %>% select(AccSpeciesName, Family) %>% semi_join(tpl_dupsp)
tpl_rmfam <- tpl_dupfam %>% 
    left_join(tpl_fam) %>% 
    group_by(AccSpeciesName) %>% 
    filter(n != max(n))

tpl_proc2 <- anti_join(tpl_proc, tpl_rmfam)
message('Done!')

saveRDS(tpl_proc2, 'pfts_species/theplantlist.rds')
