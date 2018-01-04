library(tidyverse)
match_str <- "SLA|leaf_lifespan|mass|area"

#growth_form_ignore <- c("liana_climber", "cryptophyte")
#growth_form_nonwoody <- c("graminoid", "forb_herb", "hemicryptophyte",
                          #"therophyte", "chamaephyte", "fern",
                          #"epiphyte", "geophyte", "perennial")

species_phylo <- src_sqlite("try.sqlite") %>% tbl("species_phylo") %>% collect()

plant_attrs_raw <- readRDS("processed/pfts/leaf_type.rds") %>%
    full_join(readRDS("processed/pfts/phenology.rds")) %>%
    full_join(readRDS("processed/pfts/ps_pathway.rds")) %>%
    full_join(readRDS("processed/pfts/growth_form.rds")) %>%
    full_join(readRDS("processed/pfts/n_fixation.rds")) %>%
    full_join(readRDS("processed/pfts/climate_zone.rds")) %>%
    left_join(species_phylo)

plant_attrs_raw %>% count(growth_form, sort = TRUE)

woody_gf <- c("woody", "tree", "shrub")

plant_attrs <- plant_attrs_raw %>% 
    mutate(woodiness = case_when(!is.na(.$growth_form) & .$growth_form %in% woody_gf ~ "woody",
                                 !is.na(.$growth_form) & !(.$growth_form %in% woody_gf) ~ "nonwoody",
                                 TRUE ~ NA_character_))

source("pft_schemes.R")
pfts <- plant_attrs %>%
    rowwise() %>%
    mutate(jules1 = jules1_assign(growth_form, ps_pathway, leaf_type),
           jules2 = jules2_assign(growth_form, ps_pathway, leaf_type, phenology, climate_zone),
           clm45 = clm45_assign(growth_form, ps_pathway, leaf_type, phenology, climate_zone),
           custom = custom_assign(growth_form, ps_pathway, woodiness, phenology, leaf_type, n_fixation, climate_zone)) %>% 
    "class<-"(c("tbl_df", "data.frame"))

#pftcols <- c("jules1", "jules2", "clm45", "custom")
pftcols <- c("clm45")

saveRDS(pfts, "processed/species/all_pfts.rds")

distinct_pfts <- pfts %>%
    filter(!is.na(clm45)) %>%
    distinct(AccSpeciesID, clm45)
    #filter(!is.na(jules1), !is.na(jules2), !is.na(clm45), !is.na(custom)) %>%
    #distinct(AccSpeciesID, jules1, jules2, clm45, custom)

traits_fill <- readRDS("processed/traits/trait_data.rds")

traits_pfts <- left_join(traits_fill, distinct_pfts)

saveRDS(traits_pfts, file = "processed/traits/traits_pfts.rds")

traits_analysis <- traits_pfts %>%
    filter_at(vars(one_of(pftcols)), all_vars(!is.na(.))) %>%
    filter_at(vars(matches(match_str)), any_vars(!is.na(.))) %>%
    select(ObservationID, AccSpeciesID, one_of(pftcols), which(sapply(., is_double)))

saveRDS(traits_analysis, file = "processed/traits/traits_analysis.rds")
############################################################

#plant_attrs %>% count(phenology, sort = TRUE)

#plant_attrs %>% filter(is.na(growth_form))

#plant_attrs %>% filter(phenology == "deciduous", leaf_type == "needle") %>% count(growth_form)

