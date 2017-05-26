source('common.R')

# TraitID 43 -- Leaf type
# Data IDs:
#   - 48 -- Leaf type
#   - 2416:2418 -- Leaf type: broad, needle, scale
#
# TraitID 154 -- Leaf shape
# Data IDs:
#   - 18 -- Leaf compoundness
#   - 471 -- Leaf shape

lt1_long <- trydat %>% 
    filter(DataID == 48) %>% 
    collect(n = Inf)

lt1_raw <- raw_string(lt1_long)

lt1_proc <- lt1_raw %>% 
    mutate(leaf_type = case_when(grepl('^broad(-)?leaved$', .$rawstring) ~ 'broad',
                                 grepl('^needle(-)?leaved$', .$rawstring) ~ 'needle',
                                 grepl('needle(-)?leaved', .$rawstring) ~ 'needle',
                                 grepl('broad(-)?leaved', .$rawstring) ~ 'broad',
                                 .$rawstring == 'b' ~ 'broad',
                                 .$rawstring == 'c' ~ 'needle',
                                 .$rawstring == 'n' ~ 'needle',
                                 grepl('fine-leaved', .$rawstring) ~ 'needle',
                                 grepl('conifer', .$rawstring) ~ 'needle',
                                 TRUE ~ NA_character_
                                 ))
lt1_proc %>% filter(is.na(leaf_type)) %>% count(rawstring, sort = TRUE)

#lt1_long %>% 
    #filter(tolower(trimws(OrigValueStr)) %in% c('*')) %>% 
    #distinct(AccSpeciesID) %>% 
    #inner_join(lt1_proc) %>% 
    #count(leaf_type)

lt1_final <- lt1_proc %>% 
    filter(!is.na(leaf_type)) %>% 
    distinct(AccSpeciesID, leaf_type)

############################################################

lt2_long <- trydat %>% 
    filter(DataID %in% 2416:2418) %>% 
    collect(n = Inf) %>% 
    anti_join(lt1_final)

lt2_wide <- lt2_long %>% 
    distinct(AccSpeciesID, ObservationID, DataID, OrigValueStr) %>% 
    spread(DataID, OrigValueStr) %>% 
    rename(broad = `2416`, needle = `2417`, scale = `2418`) %>% 
    select(-ObservationID) %>% 
    distinct()

lt2_proc <- lt2_wide %>% 
    mutate_at(vars(-AccSpeciesID), (function(x) x == "yes")) %>% 
    # NOTE: Scale leaves are lumped with "needle"
    mutate(leaf_type = case_when(.$scale ~ 'needle',
                                 .$needle ~ 'needle',
                                 .$broad ~ 'broad'))

lt2_final <- lt2_proc %>% 
    filter(!is.na(leaf_type)) %>% 
    distinct(AccSpeciesID, leaf_type) %>% 
    # Remove conflicts
    anti_join(count(., AccSpeciesID) %>% filter(n > 1)) %>% 
    full_join(lt1_final)

check_unique_species(lt2_final)

#lt2_final %>% count(AccSpeciesID) %>% filter(n > 1)

############################################################

count(lt2_final, leaf_type, sort = TRUE)

write_csv(lt2_final, 'attributes/leaf_type.csv')

#trydat %>% 
    #filter(OrigValueStr %in% c('broadleaf', 'needleleaf', 'broad', 'needle')) %>% 
    #count(DataID, sort = TRUE)

#datanames %>% 
    #filter(DataID %in% c(18, 471))

#lt1_long %>% count(ReferenceID)
#lt1_long %>% distinct() %>% count(ReferenceID)

#refs %>% filter(ReferenceID == 154) %>% collect() %>% .[[1]]

#datanames %>% 
    #filter(TraitName %like% "%Leaf%" | TraitName %like% "%leaf%") %>% 
    #distinct(TraitID, TraitName) %>% 
    #collect(n = Inf) %>% 
    #filter(grepl('shape|morphology|type', TraitName))
    

#datanames %>% filter(TraitID == 48)
