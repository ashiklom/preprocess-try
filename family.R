source('common.R')

species <- collect(species)

tpl_proc <- readRDS('theplantlist.rds')

#count(species)
#semi_join(species, tpl_proc) %>% count()
#anti_join(species, tpl_proc) %>% count()

message('Fixing species name encoding...')
species_fixenc <- species %>% 
    mutate(AccSpeciesName = stringi::stri_trans_general(AccSpeciesName, 'latin-ascii'))
message('Done!')

message('Merging species with ThePlantList data...')
species_merge <- species_fixenc %>% 
    left_join(tpl_proc) %>% 
    mutate(Family = recode(Family,
                           `Isoëtaceae` = 'Isoetaceae',
                           `Athyriaceae` = 'Aspleniaceae',
                           `Compositae` = 'Asteraceae',
                           `Leguminosae` = 'Fabaceae'
                           )) %>% 
    mutate(Family = case_when(!is.na(.$Family) ~ .$Family,
                              is.na(.$Family) & .$AccSpeciesName == 'Poaceae sp' ~ 'Poaceae',
                              is.na(.$Family) & .$AccSpeciesName == 'Fabaceae sp' ~ 'Fabaceae',
                              is.na(.$Family) & .$AccSpeciesName == 'Carex sp' ~ 'Cyperaceae',
                              is.na(.$Family) & .$AccSpeciesName == 'Populus sp' ~ 'Salicaceae',
                              is.na(.$Family) & .$AccSpeciesName == 'Salix sp' ~ 'Salicaceae',
                              is.na(.$Family) & .$AccSpeciesName == 'Protium sp' ~ 'Burseraceae',
                              is.na(.$Family) & .$AccSpeciesName == 'Hieracium pilosella' ~ 'Asteraceae',
                              is.na(.$Family) & .$AccSpeciesName == 'Hammada scoparia' ~ 'Amaranthaceae',
                              is.na(.$Family) & .$AccSpeciesName == 'Maxillaria uncata' ~ 'Orchidaceae',
                              is.na(.$Family) & .$AccSpeciesName == 'Dicranopteris dichotoma' ~ 'Gleicheniaceae',
                              is.na(.$Family) & .$AccSpeciesName == 'Triticum sp' ~ 'Poaceae',
                              is.na(.$Family) & .$AccSpeciesName == 'Amphicarpa bracteata' ~ 'Fabaceae',
                              is.na(.$Family) & .$AccSpeciesName == 'Coussarea racemosa' ~ 'Rubiaceae',
                              is.na(.$Family) & .$AccSpeciesName == 'Citrofortunella mitis' ~ 'Rutaceae',
                              is.na(.$Family) & .$AccSpeciesName == 'Eucalyptus sp' ~ 'Myrtaceae',
                              is.na(.$Family) & .$AccSpeciesName == 'Thymus polytrichus' ~ 'Lamiaceae',
                              is.na(.$Family) & .$AccSpeciesName == 'Achnatherum splendens' ~ 'Poaceae',
                              is.na(.$Family) & .$AccSpeciesName == 'Jessenia bataua' ~ 'Arecaceae',
                              is.na(.$Family) & .$AccSpeciesName == 'Digitalis micrantha' ~ 'Plantaginaceae',
                              TRUE ~ NA_character_))
message('Done!')

families <- species_merge %>% 
    filter(!is.na(Family)) %>% 
    distinct(Family)

phylo_db <- src_sqlite('itis_taxonomy.sqlite')

# Add new families to phylogeny database
missed_families <- tribble(
    ~Family, ~Order,
    'Lactoridaceae', 'Piperales',
    'Hypodematiaceae', 'Polypodiales',
    'Nephrolepidaceae', 'Polypodiales',
    'Cystopteridaceae', 'Polypodiales'
                           ) %>% 
    dbhelpers::db_merge_into(phylo_db, 'order_family', ., 'Family')


itis_families <- tbl(phylo_db, 'order_family') %>% collect()

#anti_join(families, itis_families)

species_tax <- species_merge %>% 
    left_join(itis_families)

species_tax %>% 
    count(AccSpeciesName) %>% 
    filter(n > 1)

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
