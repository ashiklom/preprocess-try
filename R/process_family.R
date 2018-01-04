source(here::here('R/common.R'))

tol_tax <- readRDS('processed/species/family_taxonomy.rds')

species_tax <- left_join(species_merge, tol_tax)

species_tax %>% 
    filter(is.na(tol_Family)) %>% 
    arrange(desc(N))

#species_tax %>% 
    #count(AccSpeciesName) %>% 
    #filter(n > 1)

# Write to new table in TryDB 
if (db_has_table(trydb$con, 'species_phylo')) {
    DBI::dbSendQuery(trydb$con, 'DROP TABLE species_phylo')
}
db_insert_into(trydb$con, 'species_phylo', species_tax)
file.create('.family', showWarnings = FALSE)

# DataIDs:
#   - 846 -- Family
#   - 1490 -- Family APG
#   - 1730 -- Subfamily

#family_long <- trydat %>% 
    #filter(DataID == 846) %>% 
    #collect(n = Inf)

#family_dist <- family_long %>% 
    #distinct(AccSpeciesID, OrigValueStr) %>% 
    #rename(family = OrigValueStr)

#family_dist %>% count(AccSpeciesID) %>% filter(n > 1)

#datanames %>% 
    #filter(DataName %like% "%Family%")
