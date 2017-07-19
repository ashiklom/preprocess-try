source('common.R')

species <- collect(species)

tpl_proc <- readRDS('pfts_species/theplantlist.rds')

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

saveRDS(species_merge, 'tps_species.rds')
