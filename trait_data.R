source("common.R")

# Data IDs:
#   - 13 -- leaf_lifespan
#   - 12 -- specific leaf area (SLA) -- 1/LMA
#   - 64 -- leaf mass per area (LMA) (NOTE: Based on units, this is stil SLA)
#   - 15 -- Nmass
#   - 65 -- Narea
#   - 16 -- Pmass
#   - 66 -- Parea
#   - 71 -- Rdmass  (at 25 C)
#   - 549 -- Vcmax_mass (at 25 C)
#   - 550 -- Vcmax_area (at 25 C)
#   - 2368 -- Rdarea

# Other information:
#   - 59 -- Latitude
#   - 60 -- Longitude

data_ids <- c(
  "leaf_lifespan" = 13,
  "SLA" = 12,
  "LMA" = 64,	## Called LMA, but based on units, it's still SLA
  "Nmass" = 15,
  "Narea" = 65,
  "Pmass" = 16,
  "Parea" = 66,
  #"Aarea" = 2356,
  #"Gs" = NA,       ## Not sure about this; stomatal conductance
  #"Rdmass" = 46,		## Respiration at leaf temperature
  #"Rdmass" = 70,   ## Looks like 70:72 are duplicates from same dataset
  "Rdmass" = 71,    ## Respiration at 25C
  #"Rdmass" = 72,
  #"Rdmass" = 1453,
  #"Rdarea" = 69,   ## Respiration at leaf temperature
  #"Rdarea" = 1456,
  #"Rdarea" = 2285,
  #"Rdarea" = 2368,
  #"Rdarea" = 2375,
  #"Rdarea" = 2381,
  "Vcmax_mass" = 549,   ## 25C
  "Vcmax_area" = 550,   ## 25C
  "Jmax_mass" = 664,    ## ambient
  "Jmax_mass" = 2369,   ## 25C
  "Jmax_mass" = 2383,   ## 25C
  "Jmax_area" = 665,    ## ambient
  "Jmax_area" = 1000,   ## unclear
  "Jmax_area" = 1119,   ## "potential" Jmax -- unclear
  "Jmax_area" = 2351,   ## 25C
  "Jmax_area" = 2372,   ## unclear
  "Jmax_area" = 2379,   ## 25C
  "Latitude" = 59,
  "Longitude" = 60
  #"Temperature_measurement" = 51,
  #"Temperature_measurement" = 1666
)
data_ids_noname <- unname(data_ids)

message("Extracting desired traits from TRY")
traits_long <- trydat %>%
  filter(DataID %in% data_ids_noname) %>%
  mutate(StdValue = if_else(DataID %in% c(51, 1666), as.numeric(OrigValueStr), StdValue)) %>%
  select(ObservationID, AccSpeciesID, DataID, StdValue, UnitName, ReferenceID, DatasetID) %>%
  collect(n = Inf)

message("Filtering to present StdValues and recoding trait names as factors")
traits_proc <- traits_long %>%
  filter(!is.na(StdValue)) %>%
  mutate(trait = names(data_ids[match(DataID, data_ids)]) %>% factor)

message("Writing reference list")
references <- traits_proc %>%
  count(ReferenceID, sort = TRUE)

write_csv(references, "traits/references.csv")

# Print units
distinct(traits_proc, trait, UnitName) %>% write_csv("trait_units.csv")

message("Converting traits from long to wide")
traits_unique <- traits_proc %>%
  select(DatasetID, ObservationID, AccSpeciesID, trait, StdValue) %>%
  # Aggregate observations by mean
  group_by(DatasetID, ObservationID, AccSpeciesID, trait) %>%
  summarize(value = mean(StdValue, na.rm = TRUE)) %>%
  ungroup()

traits_wide <- traits_unique %>%
  spread(trait, value)

# Perform trait conversions
fill_prod <- function(tofill, fillfrom, multby) {
  case_when(
    !is.na(tofill) ~ tofill,
    is.na(tofill) & !is.na(fillfrom) & !is.na(multby) ~ fillfrom * multby,
    TRUE ~ NA_real_
  )
}

match_str <- "SLA|LMA|leaf_lifespan|mass|area"
trait_list <- c("leaf_lifespan", "SLA",
                "Nmass", "Narea",
                "Pmass", "Parea",
                "Rdmass", "Rdarea",
                "Vcmax_mass", "Vcmax_area",
                "Jmax_mass", "Jmax_area")

message("Filling trait values based on LMA/SLA products")
traits_fill <- traits_wide %>%
  mutate(
    SLA = case_when(
      !is.na(.$SLA) ~ .$SLA,
      !is.na(.$LMA) ~ .$LMA,
      TRUE ~ NA_real_
    ),
    LMA = 1/SLA
    ) %>%
  mutate(
    Nmass = fill_prod(Nmass, Narea, SLA),
    Pmass = fill_prod(Pmass, Parea, SLA),
    #Rdmass = fill_prod(Rdmass, Rdarea, SLA),
    Vcmax_mass = fill_prod(Vcmax_mass, Vcmax_area, SLA),
    Jmax_mass = fill_prod(Jmax_mass, Jmax_area, SLA),
    Narea = fill_prod(Narea, Nmass, LMA),
    Parea = fill_prod(Parea, Pmass, LMA),
    #Rdarea = fill_prod(Rdarea, Rdmass, LMA),
    Rdarea = Rdmass * LMA,
    Vcmax_area = fill_prod(Vcmax_area, Vcmax_mass, LMA),
    Jmax_area = fill_prod(Jmax_area, Jmax_mass, LMA)
  ) %>%
  mutate_at(vars(one_of(trait_list)), function(x) {x[x < 0] = NA; x}) %>%
  # Fix unit mismatch for certain datasets
  mutate(
    Jmax_mass = case_when(
      .$DatasetID %in% c(77, 216, 255) ~ .$Jmax_mass / 1000,
      TRUE ~ .$Jmax_mass
    )
  )

message("Removing fully missing and duplicate rows")
traits_final <- traits_fill %>%
  # Remove rows where all traits are missing
  filter_at(vars(one_of(trait_list)), any_vars(!is.na(.))) %>%
  # Remove rows where all traits have the same value
  filter(!duplicated(select(., -DatasetID, -ObservationID, -AccSpeciesID)))

message("Non-missing value counts of each trait:")
traits_final %>%
  summarize_at(vars(one_of(trait_list)), ~sum(!is.na(.))) %>%
  glimpse()

#traits_final %>% 
  #summarize_at(vars(matches(match_str)), ~n_distinct(.)) %>% 
  #glimpse()

message("Saving final traits table")
saveRDS(traits_final, file = "traits/trait_data.rds")

traits_final_long <- traits_final %>%
  gather(trait, value, -DatasetID, -ObservationID, -AccSpeciesID) %>%
  filter(!is.na(value), !(trait %in% c("Latitude", "Longitude")))

diag_plot <- function(yval) {
  yvalq <- enquo(yval)
  ggplot(traits_final_long) +
    aes(x = factor(DatasetID)) +
    aes_(y = yvalq) +
    geom_boxplot() +
    facet_wrap(~trait, scales = "free")
}

distr_plot <- function(yval) {
  yvalq <- enquo(yval)
  ggplot(traits_final_long) +
    aes_(x = yvalq) +
    geom_density() +
    facet_wrap(~trait, scales = "free")
}

pdf("diagnostics/traits_by_datasetid.pdf", width = 15, height = 15)
diag_plot(value)
diag_plot(log10(value))
dev.off()

pdf("diagnostics/trait_distributions.pdf", width = 15, height = 15)
distr_plot(value)
distr_plot(log10(value))
dev.off()

############################################################
