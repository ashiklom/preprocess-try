# Get growth form for each species

source(here::here('R/common.R'))

species_phylo <- tbl(trydb, 'species_phylo') %>% collect()
gf_map <- read_csv('pft_data/growth_form.csv')

# Trait ID 42 -- Plant growth form
# Data IDs:
#   47 -- Plant growth form   
#   1297 -- Growth form   
#   1303 -- Low Growing Grass
#   1304 -- Shape and orientation
#   1425 -- Gymnosperm/Angiosperm/Tree/Herb
#   1741 -- Plant growth form 2
#   2390:2402 -- Life form

#datanames %>% 
    #filter(TraitID == 42, DataName %like% "%ucculen%") %>% 
    #select(DataName, DataID) %>% 
    #print(n = 20)

#datanames %>% 
    ##filter(TraitName %like% '%rowth%') %>% 
    ##filter(TraitName %like% '%rowth form%') %>% 
    #distinct(TraitName, TraitID)

#datanames %>% filter(DataID == 2481)
#refs %>% filter(ReferenceID == 53) %>% collect() %>% .[['Reference']]

#lookup <- function(did, rid, string, data, fname) {
    #data %>% 
        #filter(DataID == did, ReferenceID == rid, OrigValueStr == string) %>% 
        ##filter(DataID == 47, ReferenceID == 40, grepl('T resp', OrigValueStr)) %>% 
        #distinct(AccSpeciesID) %>% 
        #left_join(raw_gf %>% left_join(gf_long)) %>% 
        #count(growth_form, sort = TRUE)
#}

#lookup(47, 200, 'P', gf_long)

gf_long <- trydat %>% 
    filter(TraitID == 42) %>% 
    collect(n = Inf)

gf_proc <- inner_join(gf_long, gf_map)

set_gf <- function(growth_form) {
    common_gf <- c('succulent', 'liana_climber', 'fern', 'crop', 
                   'tree', 'shrub', 'woody', 
                   'forb', 'graminoid')
    gf_sub <- growth_form[growth_form %in% common_gf]
    if (length(gf_sub) > 0) {
        return(most_frequent(gf_sub, common_gf))
    } else {
        return('n/a')
    }
}

#c3_crop_species <- c('Avena sativa',       # Oat, C3
                     #'Hordeum vulgare',    # Barley, C3
                     #'Secale cereale',     # Rye, C3
                     #'Gycine max',         # Soybean, C3
                     ## Rice, C3
                     #'Oryza sativa',
                     #'Oryza glaberrima'
                     #)

#c3_crop_genera <- c('Triticum'              # Wheat, C3
                    #)

#c3_crop_ids <- species_phylo %>% 
    #filter(AccSpeciesName %in% c3_crop_species | Genus %in% c3_crop_genera) %>% 
    #distinct(AccSpeciesID) %>% 
    #pull(AccSpeciesID)

#c4_crop_species <- c('Zea mays',           # Maize, C4
                     #'Panicum virganum',   # Switchgrass, C4
                     ## Millets, C4
                     #'Eleusine coracana',
                     #'Panicum miliaceum',
                     #'Pennisetum glaucum',
                     #'Setaria italica',
                     ## Fonio
                     #'Digitaria exilis',
                     #'Digitaria iburua',
                     ## Sugarcane
                     #'Saccharum officinarum',
                     #'Saccharum barberi'
                     #)

#c4_crop_genera <- c('Sorghum'              # Sorghum, C4
                 #)

#c4_crop_ids <- species_phylo %>% 
    #filter(AccSpeciesName %in% c4_crop_species | Genus %in% c4_crop_genera) %>% 
    #distinct(AccSpeciesID) %>% 
    #pull(AccSpeciesID)

message('Processing growth form...')
gf_species <- 
    gf_proc %>% 
    filter(growth_form != 'n/a') %>% 
    group_by(AccSpeciesID) %>% 
    summarize(growth_form = set_gf(growth_form)) %>% 
    #mutate(growth_form = case_when(.$AccSpeciesID %in% c4_crop_ids ~ 'c4_crop',
                                   #.$AccSpeciesID %in% c3_crop_ids ~ 'c3_crop',
                                   #TRUE ~ .$growth_form)) %>% 
    filter(growth_form != 'n/a')
    #mutate(woody = most_frequent(growth_form == 'woody', TRUE),
           #liana = most_frequent(growth_form == 'liana_climber', TRUE),
           #succulent = most_frequent(growth_form == 'succulent', TRUE)) %>% 
    #mutate(growth_form_final = case_when(.$liana ~ 'liana_climber',
                                         #.$succulent ~ 'succulent',
                                         #.$woody ~ 'woody',
                                         #TRUE ~ most_frequent(.$growth_form)
                                         #))
gf_species %>% ungroup %>% count(growth_form, sort = TRUE)
#gf_species %>% filter(growth_form == 'n/a') %>% count(growth_form, sort = TRUE)

gf_missing <- anti_join(species_phylo, gf_species) %>% 
    left_join(gf_proc) %>% 
    group_by(AccSpeciesID, Family, Genus) %>% 
    summarize(gf_string = paste(sort(unique(growth_form)), collapse = '|')) %>% 
    ungroup()
#count(gf_missing, gf_string, sort = TRUE) %>% print(n = 15)
count(gf_missing, Family, sort = TRUE) %>% print(n = 15)

graminoid_families <- c('Poaceae', 'Cyperaceae', 'Juncaceae')
herb_string <- 'herb|cryptophyte|therophyte|chamaephyte|hydrophyte|geophyte|perennial'
forb_families <- c('Orchidaceae')

gf_filled <- gf_missing %>% 
    mutate(growth_form = case_when(!is.na(.$Family) & .$Family %in% graminoid_families ~ 'graminoid',
                                   grepl(herb_string, .$gf_string) ~ 'forb',
                                   !is.na(.$Family) & .$Family %in% forb_families ~ 'forb',
                                   !is.na(.$Genus) & .$Genus == 'Stirlingia' ~ 'forb',
                                   !is.na(.$Genus) & !is.na(.$Family) & 
                                       .$Family == 'Proteaceae' & !grepl('Stirlingia', .$Genus) ~ 'woody',
                                   TRUE ~ NA_character_)) %>% 
    distinct(AccSpeciesID, growth_form) %>% 
    filter(!is.na(growth_form))

gf_species <- gf_species %>% 
    anti_join(gf_missing) %>% 
    full_join(gf_filled)

gf_species %>% ungroup %>% count(growth_form, sort = TRUE)
saveRDS(gf_species, 'processed/pfts/growth_form.rds')

#gf_species %>% 
    #count(growth_form, sort = TRUE) %>% 
    #print(n = 30)

#prep_gf <- function(input, fname) {
    #input %>% 
        #count(DataID, DataName, OrigValueStr, ReferenceID, sort = TRUE) %>% 
        #ungroup() %>% 
        #left_join(datanames %>% collect()) %>% 
        #select(DataID, DataName, ReferenceID, OrigValueStr, n)

#}


#gf_proc <- gf_long %>% 
    #filter(!(DataID %in% c(2390:2402) & OrigValueStr == 'no')) %>% 
    #mutate(growth_form = case_when(grepl('^ *tree|shrub|woody|shrub/tree *$', .$OrigValueStr, ignore.case = TRUE) ~ 'woody',
                                   #grepl('^ *shrub/tree *$', .$OrigValueStr, ignore.case = TRUE) ~ 'forb_herb',
                                   #grepl('^ *herb|forb(s)?|forb-annual|forb/herb *$', .$OrigValueStr, ignore.case = TRUE) ~ 'forb_herb',
                                   #grepl('^ *grass|graminoid|grasses&sedges *$', .$OrigValueStr, ignore.case = TRUE) ~ 'forb_herb',
                                   #grepl('^ *climber *$', .$OrigValueStr, ignore.case = TRUE) ~ 'liana_climber',
                                   #grepl('^ *epiphyte *$', .$OrigValueStr, ignore.case = TRUE) ~ 'epiphyte',
                                   #grepl('^ *fern *$', .$OrigValueStr, ignore.case = TRUE) ~ 'fern',
                                   #.$DataID == 2390 & .$OrigValueStr == 'yes' ~ 'woody',
                                   #.$DataID == 2391 & .$OrigValueStr == 'yes' ~ 'woody',
                                   #.$DataID == 2392 & .$OrigValueStr == 'yes' ~ 'woody',
                                   #.$DataID == 2393 & .$OrigValueStr == 'yes' ~ 'woody',
                                   #.$DataID == 2394 & .$OrigValueStr == 'yes' ~ 'woody',
                                   #.$DataID == 2395 & .$OrigValueStr == 'yes' ~ 'liana_climber',
                                   #.$DataID == 2396 & .$OrigValueStr == 'yes' ~ 'liana_climber',
                                   #.$DataID == 2397 & .$OrigValueStr == 'yes' ~ 'forb_herb',
                                   #.$DataID == 2398 & .$OrigValueStr == 'yes' ~ 'geophyte',
                                   #.$DataID == 2399 & .$OrigValueStr == 'yes' ~ 'graminoid',
                                   #.$DataID == 2400 & .$OrigValueStr == 'yes' ~ 'succulent',
                                   #.$DataID == 2401 & .$OrigValueStr == 'yes' ~ 'fern',
                                   #.$DataID == 2402 & .$OrigValueStr == 'yes' ~ 'epiphyte',
                                   ## 2481 -- Leaf succulence
                                   #.$DataID == 2481 & .$OrigValueStr == 'yes' ~ 'succulent',
                                   #TRUE ~ NA_character_
                                   #))
#gf_proc %>% 
    #filter(is.na(growth_form),
           #!(DataID == 47 & ReferenceID == 126),
           #!(DataID %in% c(2403:2411, ))) %>% 
    #count(DataID, OrigValueStr, ReferenceID, sort = TRUE) %>% 
    #left_join(datanames %>% collect()) %>% 
    #print(n = 20)

#gf1_long <- trydat %>% 
    #filter(DataID %in% c(2390:2402)) %>% 
    #collect(n = Inf)

#gf1_wide <- gf1_long %>% 
    #distinct(AccSpeciesID, ObservationID, DataID, OrigValueStr) %>% 
    #spread(DataID, OrigValueStr) %>% 
    #rename(tree = `2390`,
           #small_tree = `2391`,
           #shrub = `2392`,
           #erect_dwarf_shrub = `2393`,
           #prostrate_drawf_shrub = `2394`,
           #liana = `2395`,
           #climber = `2396`,
           #forb = `2397`,
           #geophyte = `2398`,
           #graminoid = `2399`,
           #succulent = `2400`,
           #fern = `2401`,
           #epiphyte = `2402`) %>% 
    #select(-ObservationID) %>% 
    #distinct()

#gf1_proc <- gf1_wide %>% 
    #mutate_at(vars(-AccSpeciesID), function(x) x == 'yes') %>% 
    #mutate(growth_form = case_when(.$succulent ~ 'succulent',
                                   #.$liana ~ 'liana_climber',
                                   #.$climber ~ 'liana_climber',
                                   #.$epiphyte ~ 'epiphyte',
                                   #.$fern ~ 'fern',
                                   #.$tree ~ 'woody',
                                   #.$small_tree ~ 'woody',
                                   #.$shrub ~ 'woody',
                                   #.$erect_dwarf_shrub ~ 'woody',
                                   #.$prostrate_drawf_shrub ~ 'woody',
                                   #.$forb ~ 'forb_herb',
                                   #.$geophyte ~ 'geophyte',
                                   #.$graminoid ~ 'graminoid',
                                   #TRUE ~ NA_character_))

#gf1_final <- gf1_proc %>% 
    #filter(!is.na(growth_form)) %>% 
    #distinct(AccSpeciesID, growth_form) %>% 
    ## Remove species with multiple distinct entries (conflicts)
    #anti_join(count(., AccSpeciesID) %>% filter(n > 1))

#check_unique_species(gf1_final)


## The Xylem Database codes:
## See here for definitions: https://en.wikipedia.org/wiki/Raunki%C3%A6r_plant_life-form
##  a T -- therophyte -- therophyte
##  b H -- hemicryptophyte, including geophytes -- hemicryptophyte
##  c ch -- chamaephytes -- chamaephyte (woody base, but really short; no more than 25 cm above ground)
##  d Z -- dwarf shrub -- woody
##  e N -- shrub -- woody
##  f P -- tree -- woody
##  g -- liana -- liana_climber
##  i succ -- succulent -- succulent
##  k climb -- climber -- liana_climber
##  m hel -- helophyte -- cryptophyte
##  n hyd -- hydrophyte -- cryptophyte
##
## Not sure how to classify (hemi)cryptophytes, chamaephyte, therophytes. Will 
## see how many species and traits, then subdivide if possible.

#gf2_long <- trydat %>% 
    #filter(ReferenceID == 196, DataID == 47) %>% 
    #distinct(AccSpeciesID, OrigValueStr) %>% 
    #collect(n = Inf) %>% 
    #anti_join(gf1_final)

##gf2_long %>% count(OrigValueStr) %>% arrange(desc(n))

#gf2_raw <- raw_string(gf2_long, '|', lower = FALSE)

#gf2_proc <- gf2_raw %>% 
    #mutate(growth_form = case_when(grepl('succ', .$rawstring) ~ 'succulent',
                                   #.$rawstring == 'g' ~ 'liana_climber',
                                   #.$rawstring == 'g L' ~ 'liana_climber',
                                   #.$rawstring %in% c('n hyd', 'n Hyd', 'hyd') ~ 'cryptophyte',
                                   #.$rawstring %in% c('hel', 'Hel') ~ 'cryptophyte',
                                   #.$rawstring == 'k Climber' ~ 'liana_climber',
                                   #.$rawstring %in% c('b H', 'h', 'H', 'H|h') ~ 'hemicryptophyte',
                                   #.$rawstring %in% c('e N', 'f P', 'n|p', 'p', 'n') ~ 'woody',
                                   #.$rawstring %in% c('a T', 'therophyte') ~ 'therophyte',
                                   #.$rawstring %in% c('c c', 'c C', 'Ch', 'ch', 'c', 'C') ~ 'chamaephyte',
                                   #grepl('b (H|h)', .$rawstring) ~ 'hemicryptophyte',
                                   #grepl('m (H|h)el', .$rawstring) ~ 'cryptophyte',
                                   #grepl('(T|t)herophyte', .$rawstring) ~ 'therophyte',
                                   #grepl('(P|p)erennial', .$rawstring) ~ 'perennial',
                                   #grepl('d (z|Z)', .$rawstring) ~ 'woody',
                                   #grepl('(s|S)hrub|(T|t)ree', .$rawstring) ~ 'woody',
                                   #grepl('(z|Z)', .$rawstring) ~ 'woody',
                                   #grepl('a (t|T)', .$rawstring) ~ 'therophyte',
                                   #grepl('(\\be N\\b|\\bf P\\b)', .$rawstring) ~ 'woody',
                                   #.$rawstring == 't' ~ 'therophyte',
                                   #TRUE ~ NA_character_
                                   #))
#gf2_proc %>% filter(is.na(growth_form)) %>% count(rawstring, sort = TRUE) %>% print()
#gf2_proc %>% filter(is.na(growth_form)) %>% count() %>% print()

#gf2_final <- filter(gf2_proc, !is.na(growth_form)) %>% 
    #distinct(AccSpeciesID, growth_form) %>% 
    #full_join(gf1_final)

#check_unique_species(gf2_final)

#############################################################

#gf3_long <- trydat %>% 
    #filter(DataID %in% c(47, 1297, 1425, 1741)) %>% 
    #distinct(AccSpeciesID, OrigValueStr) %>% 
    #collect(n = Inf) %>% 
    #anti_join(gf2_final)

#gf3_raw <- raw_string(gf3_long, sep = '|')

#gf3_proc <- gf3_raw %>% 
    #mutate(growth_form = case_when(.$rawstring == 'grass' ~ 'graminoid',
                                   #.$rawstring %in% c('herb', 'forb', 'forbs', 'herbaceous') ~ 'forb_herb',
                                   #.$rawstring %in% c('tree', 'tree|tree', 't|tree|tree', 't|tree') ~ 'woody',
                                   #.$rawstring %in% c('shrub', 'shrub|shrub', 's|shrub|shrub') ~ 'woody',
                                   #.$rawstring %in% c('shrub|tree', 'tree|woody') ~ 'woody',
                                   #.$rawstring == 'climber' ~ 'liana_climber',
                                   #.$rawstring == 'succulent' ~ 'succulent',
                                   #grepl('(hemi(-)?)?epip[hj][yi]te', .$rawstring) ~ 'epiphyte',
                                   #.$rawstring == 'geophyte' ~ 'geophyte',
                                   #.$rawstring == 'aquatic' ~ 'cryptophyte',
                                   #grepl('shru(b)?|tree|scrub', .$rawstring) ~ 'woody',
                                   #grepl('climber|lian(n)?a|vine', .$rawstring) ~ 'liana_climber',
                                   #grepl('grass|gram(inoid)?', .$rawstring) ~ 'graminoid',
                                   #grepl('fern', .$rawstring) ~ 'fern',
                                   #grepl('forb|herb', .$rawstring) & !grepl('grass', .$rawstring) ~ 'forb_herb',
                                   #.$rawstring %in% c('a', 't', 's') ~ 'woody',
                                   #TRUE ~ NA_character_
                                   #))
#gf3_proc %>% filter(is.na(growth_form)) %>% count(rawstring, sort = TRUE)
#gf3_proc %>% filter(is.na(growth_form)) %>% count() %>% .[['n']]

##gf3_long %>% 
    ##filter(tolower(trimws(OrigValueStr)) %in% c('l')) %>% 
    ##distinct(AccSpeciesID) %>% 
    ##inner_join(gf3_proc) %>% 
    ##count(growth_form)

#gf3_final <- filter(gf3_proc, !is.na(growth_form)) %>% 
    #distinct(AccSpeciesID, growth_form) %>% 
    #full_join(gf2_final)

#check_unique_species(gf3_final)

#gf3_final %>% count(growth_form, sort = TRUE)

#############################################################
## Figure out remaining traits based on phylogeny
#gf4_long <- trydat %>% 
    #distinct(AccSpeciesID) %>% 
    #collect(n = Inf) %>% 
    #anti_join(gf3_final)

#gf4_phylo <- gf4_long %>% 
    #left_join(species_phylo) %>% 
    #mutate(growth_form = case_when(.$Family == 'Orchidaceae' ~ 'perennial',
                                   #.$Family == 'Myrtaceae' ~ 'woody',
                                   #.$Family == 'Iridaceae' ~ 'perennial',
                                   #.$Family == 'Proteaceae' & !grepl('Stirlingia', .$AccSpeciesName) ~ 'woody',
                                   #.$Family == 'Cyperaceae' ~ 'graminoid',
                                   #.$Family == 'Poaceae' ~ 'graminoid',
                                   #.$Family == 'Lauraceae' & !grepl('Cassytha', .$AccSpeciesName) ~ 'woody',
                                   #.$Family == 'Lauraceae' & grepl('Cassytha', .$AccSpeciesName) ~ 'liana_climber',
                                   #TRUE ~ NA_character_))
#gf4_phylo %>% filter(is.na(growth_form)) %>% count(Family, sort = TRUE) %>% print(n = 20)

#write_csv(gf3_final, path = 'processed/pfts/growth_form.csv')

    ## See above note about special PFTs
    #mutate(pft = case_when(grepl('succulent|succ|cact', .$rawstring) ~ 'succulent',
                           #grepl('climber|liana|vine|lian', .$rawstring) ~ 'liana_climber',
                           #grepl('hydrophyte|submerged|aquatic|macrophyte', .$rawstring) ~ 'hydrophyte',
                           #grepl('cushion plant|epiphyte|epiphite|hemi-epi|moss', .$rawstring) ~ 'epiphyte_other',
                           #.$rawstring == 'l' ~ 'liana_climber',       # Probably 'liana'
                           #.$rawstring == 'g l' ~ 'liana_climber',     # Probably 'liana'
                           #TRUE ~ NA_character_
                           #)) %>% 
    #mutate(woodiness = case_when(!is.na(.$pft) ~ 'special_pft',
                                 #grepl('tree|woody|shrub', .$rawstring) ~ 'woody',
                                 #grepl('herb|graminoid|grass|forb|fern', .$rawstring) ~ 'non-woody',
                                 #.$rawstring == 
                                 #.$rawstring == 'e n' ~ 'woody',   # Not sure, but 307 woody, 6 non-woody
                                 #.$rawstring == 'd z' ~ 'woody',   # 
                                 #.$rawstring == 'f p' ~ 'woody',   # 148 woody, 1 special
                                 #grepl('palm', .$rawstring) ~ 'woody',
                                 #grepl('gram', .$rawstring) ~ 'non-woody',
                                 #grepl('geophyte|geop', .$rawstring) ~ 'non-woody',
                                 #grepl('shru|scrub', .$rawstring) ~ 'woody',
                                 #grepl('mid canopy', .$rawstring) ~ 'woody',
                                 ## Woody base, but most of plant is not woody
                                 #grepl('suffrutescent', .$rawstring) ~ 'non-woody',
                                 #grepl('perennial', .$rawstring) ~ 'non-woody',
                                 #grepl('\\ba\\b', .$rawstring) ~ 'woody',  # a -- Arbol, a.k.a tree
                                 #grepl('\\bt\\b', .$rawstring) ~ 'woody',  # t -- Tree
                                 #grepl('\\bs\\b', .$rawstring) ~ 'woody',  # s -- shrub
                                 #grepl('\\bh\\b', .$rawstring) ~ 'non-woody',  # h -- probably herb
                                 #.$rawstring == 'g' ~ 'non-woody', # Probably graminoid
                                 #.$rawstring == 'crops' ~ 'non-woody',
                                 ##grepl('\\bf\\b', .$rawstring) ~ 'non-woody',
                                 ##grepl('\\bg\\b', .$rawstring) ~ 'non-woody',
                                 ##.$rawstring == 'e n' ~ 'woody',   # 310 woody, 22 non-woody
                                 #TRUE ~ NA_character_
                                 #))

# TODO:
#   c c -- 58 woody, 32 non-woody, 6 special; from DataID 47, ReferenceID 196 

# DataIDs 1425, 1303, and 1304 have the following:
#   - na, no decumbent, no, no rounded, no semi-erect, no erect
# Not sure what to do with any of these, so skipping

#gf2_long <- trydat %>% 
    #filter(DataID %in% c(1425, 1303, 1304)) %>% 
    #collect(n = Inf) %>% 
    #anti_join(gf1_final)
#gf2_raw <- raw_string(gf2_long)
#gf2_final <- gf1_final


