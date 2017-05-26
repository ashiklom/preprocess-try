# Get growth form for each species

source('common.R')

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

gf1_long <- trydat %>% 
    filter(DataID %in% c(2390:2402)) %>% 
    collect(n = Inf)

gf1_wide <- gf1_long %>% 
    distinct(AccSpeciesID, ObservationID, DataID, OrigValueStr) %>% 
    spread(DataID, OrigValueStr) %>% 
    rename(tree = `2390`,
           small_tree = `2391`,
           shrub = `2392`,
           erect_dwarf_shrub = `2393`,
           prostrate_drawf_shrub = `2394`,
           liana = `2395`,
           climber = `2396`,
           forb = `2397`,
           geophyte = `2398`,
           graminoid = `2399`,
           succulent = `2400`,
           fern = `2401`,
           epiphyte = `2402`) %>% 
    select(-ObservationID) %>% 
    distinct()

gf1_proc <- gf1_wide %>% 
    mutate_at(vars(-AccSpeciesID), function(x) x == 'yes') %>% 
    mutate(growth_form = case_when(.$succulent ~ 'succulent',
                                   .$liana ~ 'liana_climber',
                                   .$climber ~ 'liana_climber',
                                   .$epiphyte ~ 'epiphyte',
                                   .$fern ~ 'fern',
                                   .$tree ~ 'woody',
                                   .$small_tree ~ 'woody',
                                   .$shrub ~ 'woody',
                                   .$erect_dwarf_shrub ~ 'woody',
                                   .$prostrate_drawf_shrub ~ 'woody',
                                   .$forb ~ 'forb_herb',
                                   .$geophyte ~ 'geophyte',
                                   .$graminoid ~ 'graminoid',
                                   TRUE ~ NA_character_))

gf1_final <- gf1_proc %>% 
    filter(!is.na(growth_form)) %>% 
    distinct(AccSpeciesID, growth_form) %>% 
    # Remove species with multiple distinct entries (conflicts)
    anti_join(count(., AccSpeciesID) %>% filter(n > 1))

check_unique_species(gf1_final)


# The Xylem Database codes:
# See here for definitions: https://en.wikipedia.org/wiki/Raunki%C3%A6r_plant_life-form
#  a T -- therophyte -- therophyte
#  b H -- hemicryptophyte, including geophytes -- hemicryptophyte
#  c ch -- chamaephytes -- chamaephyte (woody base, but really short; no more than 25 cm above ground)
#  d Z -- dwarf shrub -- woody
#  e N -- shrub -- woody
#  f P -- tree -- woody
#  g -- liana -- liana_climber
#  i succ -- succulent -- succulent
#  k climb -- climber -- liana_climber
#  m hel -- helophyte -- cryptophyte
#  n hyd -- hydrophyte -- cryptophyte
#
# Not sure how to classify (hemi)cryptophytes, chamaephyte, therophytes. Will 
# see how many species and traits, then subdivide if possible.

gf2_long <- trydat %>% 
    filter(ReferenceID == 196, DataID == 47) %>% 
    distinct(AccSpeciesID, OrigValueStr) %>% 
    collect(n = Inf) %>% 
    anti_join(gf1_final)

#gf2_long %>% count(OrigValueStr) %>% arrange(desc(n))

gf2_raw <- raw_string(gf2_long, '|', lower = FALSE)

gf2_proc <- gf2_raw %>% 
    mutate(growth_form = case_when(grepl('succ', .$rawstring) ~ 'succulent',
                                   .$rawstring == 'g' ~ 'liana_climber',
                                   .$rawstring == 'g L' ~ 'liana_climber',
                                   .$rawstring %in% c('n hyd', 'n Hyd', 'hyd') ~ 'cryptophyte',
                                   .$rawstring %in% c('hel', 'Hel') ~ 'cryptophyte',
                                   .$rawstring == 'k Climber' ~ 'liana_climber',
                                   .$rawstring %in% c('b H', 'h', 'H', 'H|h') ~ 'hemicryptophyte',
                                   .$rawstring %in% c('e N', 'f P', 'n|p', 'p', 'n') ~ 'woody',
                                   .$rawstring %in% c('a T', 'therophyte') ~ 'therophyte',
                                   .$rawstring %in% c('c c', 'c C', 'Ch', 'ch', 'c', 'C') ~ 'chamaephyte',
                                   grepl('b (H|h)', .$rawstring) ~ 'hemicryptophyte',
                                   grepl('m (H|h)el', .$rawstring) ~ 'cryptophyte',
                                   grepl('(T|t)herophyte', .$rawstring) ~ 'therophyte',
                                   grepl('(P|p)erennial', .$rawstring) ~ 'perennial',
                                   grepl('d (z|Z)', .$rawstring) ~ 'woody',
                                   grepl('(s|S)hrub|(T|t)ree', .$rawstring) ~ 'woody',
                                   grepl('(z|Z)', .$rawstring) ~ 'woody',
                                   grepl('a (t|T)', .$rawstring) ~ 'therophyte',
                                   grepl('(\\be N\\b|\\bf P\\b)', .$rawstring) ~ 'woody',
                                   .$rawstring == 't' ~ 'therophyte',
                                   TRUE ~ NA_character_
                                   ))
gf2_proc %>% filter(is.na(growth_form)) %>% count(rawstring, sort = TRUE) %>% print()
gf2_proc %>% filter(is.na(growth_form)) %>% count() %>% print()

gf2_final <- filter(gf2_proc, !is.na(growth_form)) %>% 
    distinct(AccSpeciesID, growth_form) %>% 
    full_join(gf1_final)

check_unique_species(gf2_final)

############################################################

gf3_long <- trydat %>% 
    filter(DataID %in% c(47, 1297, 1425, 1741)) %>% 
    distinct(AccSpeciesID, OrigValueStr) %>% 
    collect(n = Inf) %>% 
    anti_join(gf2_final)

gf3_raw <- raw_string(gf3_long, sep = '|')

gf3_proc <- gf3_raw %>% 
    mutate(growth_form = case_when(.$rawstring == 'grass' ~ 'graminoid',
                                   .$rawstring %in% c('herb', 'forb', 'forbs', 'herbaceous') ~ 'forb_herb',
                                   .$rawstring %in% c('tree', 'tree|tree', 't|tree|tree', 't|tree') ~ 'woody',
                                   .$rawstring %in% c('shrub', 'shrub|shrub', 's|shrub|shrub') ~ 'woody',
                                   .$rawstring %in% c('shrub|tree', 'tree|woody') ~ 'woody',
                                   .$rawstring == 'climber' ~ 'liana_climber',
                                   .$rawstring == 'succulent' ~ 'succulent',
                                   grepl('(hemi(-)?)?epip[hj][yi]te', .$rawstring) ~ 'epiphyte',
                                   .$rawstring == 'geophyte' ~ 'geophyte',
                                   .$rawstring == 'aquatic' ~ 'cryptophyte',
                                   grepl('shru(b)?|tree|scrub', .$rawstring) ~ 'woody',
                                   grepl('climber|lian(n)?a|vine', .$rawstring) ~ 'liana_climber',
                                   grepl('grass|gram(inoid)?', .$rawstring) ~ 'graminoid',
                                   grepl('fern', .$rawstring) ~ 'fern',
                                   grepl('forb|herb', .$rawstring) & !grepl('grass', .$rawstring) ~ 'forb_herb',
                                   .$rawstring %in% c('a', 't', 's') ~ 'woody',
                                   TRUE ~ NA_character_
                                   ))
gf3_proc %>% filter(is.na(growth_form)) %>% count(rawstring, sort = TRUE)
gf3_proc %>% filter(is.na(growth_form)) %>% count() %>% .[['n']]

#gf3_long %>% 
    #filter(tolower(trimws(OrigValueStr)) %in% c('l')) %>% 
    #distinct(AccSpeciesID) %>% 
    #inner_join(gf3_proc) %>% 
    #count(growth_form)

gf3_final <- filter(gf3_proc, !is.na(growth_form)) %>% 
    distinct(AccSpeciesID, growth_form) %>% 
    full_join(gf2_final)

check_unique_species(gf3_final)

gf3_final %>% count(growth_form, sort = TRUE)

write_csv(gf3_final, path = 'attributes/growth_form.csv')

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


