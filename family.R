source('common.R')

species <- collect(species)

#species_str <- species[['AccSpeciesName']] %>% 
    #stringi::stri_trans_general('latin-ascii')

## Data source 3 -- ITIS
#sp_breaks <- ceiling(seq_along(species_str) / 300)
#z <- gnr_resolve(species_str, best_match_only = TRUE, with_context = TRUE, preferred_data_sources = 3)

#writeLines(species_str, 'all_species')

library(taxize)

#tpl_get('tpl/')

tpl_list <- list.files('tpl', full.names = TRUE)

tpl_dat <- tpl_list %>% 
    map(data.table::fread, header = TRUE) %>% 
    data.table::rbindlist(fill = TRUE) %>% 
    as_data_frame()

tpl_proc <- tpl_dat %>% 
    mutate(AccSpeciesName = paste(Genus, Species)) %>% 
    select(tpl_id = ID, major_group = `Major group`, AccSpeciesName, Family, Genus, Species)

#tpl_proc %>% glimpse()

#count(species)
#semi_join(species, tpl_proc) %>% count()
#anti_join(species, tpl_proc) %>% count()

species_merge <- species %>% 
    left_join(tpl_proc) %>% 
    mutate(Family = factor(Family) %>% 
                forcats::fct_recode(Isoetaceae = 'Isoëtaceae',
                                    Aspleniaceae = 'Athyriaceae',
                                    Asteraceae = 'Compositae',
                                    Fabaceae = 'Leguminosae'
                                    ))
    mutate(Family = case_when(.$Family == 'Leguminosae' ~ 'Fabaceae'
                              TRUE ~ NA_character_))

families <- species_merge %>% 
    filter(!is.na(Family)) %>% 
    distinct(Family)

phylo_db <- src_sqlite('itis_taxonomy.sqlite')

# Add new families to phylogeny database
missed_families <- tribble(
    ~Family, ~Order,
    'Lactoridaceae', 'Piperales'
    'Hypodematiaceae', 'Polypodiales'
    'Nephrolepidaceae', 'Polypodiales'
    'Cystopteridaceae', 'Polypodiales'
                           ) #%>% 
    #dbhelpers::db_merge_into(phylo_db)


itis_families <- tbl(phylo_db, 'order_family')

anti_join(families, itis_taxonomy)

species_tax <- species_merge %>% 
    left_join(itis_taxonomy)

# Missed families:
# - Lactoridaceae -- Order: Piperales
# - Hypodematiaceae -- Order: Polypodiales
# - Nephrolepidaceae -- Order: Polypodiales
# - Cystopteridaceae -- Order: Polypodiales

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
