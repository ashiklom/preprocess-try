source(here::here('R/common.R'))

# TraitID 22 -- Photosynthetic pathway
# DataID 25 -- Photosynthetic pathway

psp_long <- trydat %>% 
    filter(TraitID == 22) %>% 
    left_join(datanames) %>% 
    collect(n = Inf)

#psp_long %>% 
    #prep_sheet %>% 
    #write_csv('pft_data/ps_pathway.csv')

#lookup(25, 205, 'Y', psp_long, 'pft_data/ps_pathway.csv', 'ps_pathway')

psp_map <- read_csv('pft_data/ps_pathway.csv')

psp_proc <- inner_join(psp_long, psp_map)

psp_species <- psp_proc %>% 
    filter(ps_pathway != 'n/a') %>% 
    group_by(AccSpeciesID) %>% 
    summarize(ps_pathway = most_frequent(ps_pathway, c('CAM', 'C4', 'C3')))

count(psp_species, ps_pathway, sort = TRUE)

saveRDS(psp_species, 'processed/pfts/ps_pathway.rds')
