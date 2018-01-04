# Get phenology for each species

source(here::here('R/common.R'))

# TraitID 37 -- "Leaf phenology type"

pheno_long <- trydat %>% 
    filter(TraitID == 37) %>% 
    left_join(datanames) %>% 
    collect(n = Inf)

#pheno_long %>% 
    #prep_sheet %>% 
    #write_csv('pft_data/phenology.csv')

#lookup(42, 154, 'W', pheno_long, 'pft_data/phenology.csv', 'phenology')

pheno_map <- read_csv('pft_data/phenology.csv')

pheno_proc <- inner_join(pheno_long, pheno_map)

pheno_proc %>% count(phenology, sort = TRUE)

pheno_species <- pheno_proc %>% 
    filter(phenology != 'n/a') %>% 
    group_by(AccSpeciesID) %>% 
    summarize(phenology = most_frequent(phenology, c('deciduous', 'evergreen')))

pheno_species %>% count(phenology, sort = TRUE)

saveRDS(pheno_species, 'processed/pfts/phenology.rds')
