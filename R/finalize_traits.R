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

filter_valid <- function(dat) {
  dat %>%
# TODO: Move these fixes to initial trait selection
# TODO: Change AccSpeciesID to species name
# TODO: Look at ObservationID 1671740
    mutate(
      SLA = case_when(
        ObservationID %in% 1674129:1674132 ~ SLA / 100,
        TRUE ~ SLA
      )
    ) %>%
    filter(
      !Nmass > 100, !Nmass < 0,
      !Pmass > 100, !Pmass < 0
    )
}


traits_analysis <- traits_pfts %>%
    filter_at(vars(one_of(pftcols)), all_vars(!is.na(.))) %>%
    filter_at(vars(one_of(trait_cols)), any_vars(!is.na(.))) %>%
    filter_valid() %>%
    select(ObservationID, AccSpeciesID, one_of(pftcols), Latitude, Longitude,
           one_of(trait_cols))

# Diagnostic plots
traits_long <- traits_analysis %>%
  select(clm45, one_of(trait_cols)) %>%
  gather("trait", "value", -clm45)

ggplot(traits_long) +
  aes(x = clm45, y = value) +
  geom_boxplot() +
  facet_wrap(~ trait, scales = "free")


saveRDS(traits_analysis, file = "processed/traits/traits_analysis.rds")
