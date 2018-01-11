library(tidyverse)
library(here)

trait_cols <- c(
  "leaf_lifespan",
  "SLA",
  "Nmass", "Narea",
  "Pmass", "Parea",
  "Rdmass", "Rdarea",
  "Vcmax_mass", "Vcmax_area",
  "Jmax_mass", "Jmax_area"
)

pftcols <- c("clm45")

traits_pfts <- readRDS(here("processed", "traits", "traits_pfts.rds"))

traits_analysis <- traits_pfts %>%
    filter_at(pftcols, all_vars(!is.na(.))) %>%
    filter_at(trait_cols, any_vars(!is.na(.))) %>%
    filter(!duplicated(.)) %>%
    select(ObservationID, AccSpeciesID, pftcols, trait_cols)

# Diagnostic plots
traits_long <- traits_analysis %>%
  select(pft = clm45, trait_cols) %>%
  gather("trait", "value", -pft)

ggplot(traits_long) +
  aes(x = pft, y = value) +
  geom_boxplot() +
  facet_wrap(~ trait, scales = "free") +
  scale_y_log10()

saveRDS(traits_analysis, file = "processed/traits/traits_analysis.rds")
