# Get phenology for each species

source('common.R')

# TraitID 37 -- "Leaf phenology type"
# Data IDs:
#  42 -- Leaf phenology type
#  226 -- Leaf phenology 2 (seasonal timing of foliage)
#  2419 -- Leaf phenology: deciduous
#  2420 -- Leaf phenology: semi-deciduous
#  2421 -- Leaf phenology: leaf exhanger
#  2422 -- Leaf phenology: evergreen

# Species availability:
#   - 92079 total species
#   - 14322 have 42
#   - 15908 have 42, 226 (+1586)
#   - 15908 have any of the IDs above

pheno1_long <- filter(trydat, DataID == 42) %>% 
    collect(n = Inf)

pheno1_raw <- raw_string(pheno1_long)
    
pheno1_proc <- pheno1_raw %>% 
    mutate(phenology = case_when(grepl('deciduous', .$rawstring) ~ 'deciduous',
                                 grepl('vernal', .$rawstring) ~ 'deciduous',
                                 grepl('aestival', .$rawstring) ~ 'deciduous',
                                 grepl('hibernal', .$rawstring) ~ 'deciduous',
                                 grepl('^(d ?)+$', .$rawstring) ~ 'deciduous',
                                 grepl('^((n|d) ?)+$', .$rawstring) ~ 'deciduous',
                                 grepl('\\bd\\b', .$rawstring) ~ 'deciduous',
                                 grepl('\\bn\\b', .$rawstring) ~ 'deciduous',
                                 grepl('evergreen', .$rawstring) ~ 'evergreen',
                                 grepl('^((e|ev) ?)+', .$rawstring) ~ 'evergreen',
                                 grepl('^((e|ev|y) ?)+', .$rawstring) ~ 'evergreen',
                                 grepl('d_ev', .$rawstring) ~ 'deciduous',
                                 TRUE ~ NA_character_))
    #(. %>% filter(is.na(phenology)) %>% distinct(rawstring) %>% .[['rawstring']] %>% print())

# TODO:
#   - "w"
#   - 1, 2, 3

pheno1_final <- filter(pheno1_proc, !is.na(phenology)) %>% 
    distinct(AccSpeciesID, phenology)

check_unique_species(pheno1_final)

############################################################

pheno2 <- filter(trydat, DataID %in% 2419:2422) %>% 
    select(ObservationID, AccSpeciesID, DataID, OrigValueStr) %>% 
    collect(n = Inf) %>% 
    anti_join(pheno1_final) %>% 
    spread(DataID, OrigValueStr) %>% 
    rename('deciduous' = `2419`,
           'semi_deciduous' = `2420`,
           'leaf_exchanger' = `2421`,
           'evergreen' = `2422`)

pheno2_proc <- pheno2 %>% 
    mutate(phenology = case_when(.$deciduous == 'yes' ~ 'deciduous',
                                 .$semi_deciduous == 'yes' ~ 'deciduous',
                                 .$leaf_exchanger == 'yes' ~ 'deciduous',
                                 .$evergreen == 'yes' ~ 'evergreen',
                                 TRUE ~ NA_character_))

pheno2_final <- filter(pheno2_proc, !is.na(phenology)) %>% 
    distinct(AccSpeciesID, phenology) %>% 
    full_join(pheno1_final) %>% 
    # Remove duplicates (conflicting entries)
    anti_join(count(., AccSpeciesID) %>% filter(n > 1))

check_unique_species(pheno2_final)

############################################################

pheno3 <- filter(trydat, DataID %in% c(42, 226, 2419:2422)) %>% 
    select(AccSpeciesID, DataID, OrigValueStr) %>% 
    collect() %>% 
    anti_join(pheno2_final)

pheno3_raw <- raw_string(pheno3)

# "Always summer green" -- 514 deciduous, 49 evergreen (assume deciduous)
# "Always persistent green" -- 142 deciduous, 192 evergreen (no assumption)
# "Always overwintering green" -- 77 deciduous, 12 evergreen (no assumption)
# "Always spring green" -- 11 deciduous, 0 evergreen (assume deciduous)
pheno3_proc <- pheno3_raw %>% 
    mutate(phenology = case_when(grepl('always summer green', .$rawstring) ~ 'deciduous',
                                 grepl('always spring green', .$rawstring) ~ 'deciduous',
                                 TRUE ~ NA_character_))

pheno3_final <- pheno3_proc %>% 
    filter(!is.na(phenology)) %>% 
    distinct(AccSpeciesID, phenology) %>% 
    full_join(pheno2_final)

pheno3_final %>% 
    semi_join(pheno3_final %>% count(AccSpeciesID) %>% filter(n > 1))

check_unique_species(pheno3_final)

pheno_counts <- count(pheno3_final, phenology)
nd <- filter(pheno_counts, phenology == 'deciduous')[['n']]
ne <- filter(pheno_counts, phenology == 'evergreen')[['n']]
total <- nd + ne
all_total <- trydat %>% distinct(AccSpeciesID) %>% collect() %>% nrow()
message(sprintf('%d species with phenology data. %d deciduous, %d evergreen.', total, nd, ne))
message(sprintf('Phenology information available for %d of %d species (%.2f%%).', 
                total, all_total, total / all_total * 100))

write_csv(pheno3_final, 'attributes/phenology.csv')

#trydat %>% 
    #filter(DataID == 226, OrigValueStr == 'always spring green') %>% 
    #distinct(AccSpeciesID) %>% 
    #collect() %>% 
    #inner_join(pheno2_final) %>% 
    #count(phenology) %>% 
    #arrange(desc(n))

#pheno3_raw %>% count(rawstring) %>% arrange(desc(n))
#pheno3_raw %>% distinct(rawstring) %>% print(n = Inf)

#pheno2_proc %>% filter(is.na(phenology))
    
#trydat %>% 
    #filter(DataID %in% c(42, 226, 2419:2422)) %>% 
    #distinct(AccSpeciesID) %>% 
    #count()

#trydat %>% 
    #filter(DataID == 226) %>% 
    #distinct(OrigValueStr)
