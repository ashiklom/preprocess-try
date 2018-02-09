library(tidyverse)
setwd(here::here())
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

source("R/pft_schemes.R")
pfts <- plant_attrs %>%
    rowwise() %>%
    mutate(jules1 = jules1_assign(growth_form, ps_pathway, leaf_type),
           jules2 = jules2_assign(growth_form, ps_pathway, leaf_type, phenology, climate_zone),
           clm45 = clm45_assign(growth_form, ps_pathway, leaf_type, phenology, climate_zone),
           custom = custom_assign(growth_form, ps_pathway, woodiness, phenology, leaf_type, n_fixation, climate_zone)) %>%
    "class<-"(c("tbl_df", "data.frame"))

saveRDS(pfts, "processed/species/all_pfts.rds")

distinct_pfts <- pfts %>%
    filter(!is.na(clm45)) %>%
    distinct(AccSpeciesID, clm45)

traits_fill <- readRDS("processed/traits/trait_data.rds")
traits_pfts <- traits_fill %>%
  left_join(distinct_pfts)

saveRDS(traits_pfts, file = "processed/traits/traits_pfts.rds")

############################################################

plot_dat <- traits_pfts %>%
  gather(trait, value, -ObservationID, -ReferenceID, -AccSpeciesID, -clm45, na.rm = TRUE) %>%
  filter(!(trait == "leaf_lifespan" & value > 200))

pdf("diagnostics/traits_by_pft.pdf")

ggplot(plot_dat) +
  aes(x = clm45, y = value, color = clm45) +
  geom_boxplot() +
  facet_wrap(~trait, scales = "free") +
  theme(axis.text.x = element_blank())

ggplot(plot_dat) +
  aes(x = clm45, y = log10(value), color = clm45) +
  geom_boxplot() +
  facet_wrap(~trait, scales = "free") +
  theme(axis.text.x = element_blank())

dev.off()
