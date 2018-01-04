source(here::here('R/common.R'))

#datanames %>% 
    #filter(TraitName %like% '%fix%') %>% 
    #select(TraitName, TraitID)

#datanames %>% 
    #filter(TraitID == 8) %>% 
    #select(DataName, DataID)

# TraitID 8 -- Nitrogen-fixation capacity
# DataID 9 -- Nitrogen-fixation capacity

nfix_long <- trydat %>% 
    filter(TraitID == 8) %>% 
    left_join(datanames) %>% 
    collect(n = Inf)

#nfix_long %>% 
    #prep_sheet %>% 
    #write_csv('pft_data/n_fixation.csv')

#lookup(9, 62, 'hp', nfix_long, 'pft_data/n_fixation.csv', 'n_fixation')

nfix_map <- read_csv('pft_data/n_fixation.csv')

nfix_proc <- inner_join(nfix_long, nfix_map)

nfix_species <- nfix_proc %>% 
    filter(n_fixation != 'n/a') %>% 
    group_by(AccSpeciesID) %>% 
    summarize(n_fixation = most_frequent(n_fixation, c('TRUE', 'FALSE'))) %>% 
    mutate(n_fixation = as.logical(n_fixation))

count(nfix_species, n_fixation, sort = TRUE)

saveRDS(nfix_species, 'processed/pfts/n_fixation.rds')
