source('common.R')

# DataID 25 -- "Photosynthetic pathway"

psp1 <- trydat %>% 
    filter(DataID == 25) %>% 
    collect(n = Inf)

psp1_raw <- raw_string(psp1)

psp1_proc <- psp1_raw %>% 
    mutate(ps_pathway = case_when(.$rawstring == 'c3' ~ 'C3',
                                  .$rawstring == 'c4' ~ 'C4',
                                  .$rawstring == 'cam' ~ 'CAM',
                                  # If there are conflicts, go in order of increasing frequency
                                  grepl('cam', .$rawstring) ~ 'CAM',
                                  grepl('c4', .$rawstring) ~ 'C4',
                                  grepl('c3', .$rawstring) ~ 'C3',
                                  TRUE ~ NA_character_
                                  ))
psp1_proc %>% filter(is.na(ps_pathway)) %>% count(rawstring, sort = TRUE)

psp1_final <- psp1_proc %>% 
    filter(!is.na(ps_pathway)) %>% 
    distinct(AccSpeciesID, ps_pathway)

check_unique_species(psp1_final)

write_csv(psp1_final, 'ps_pathway.csv')
  
#trydat %>% 
    #filter(OrigValueStr %in% c('C3', 'C4', 'c3', 'c4')) %>% 
    #count(DataID, sort = TRUE)

#datanames %>% 
    #filter(DataID %in% c(25, 235)) %>% 
    #select(DataID, DataName)
