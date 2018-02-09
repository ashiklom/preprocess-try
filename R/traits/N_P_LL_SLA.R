source(here::here("R", "common.R"))

data_ids <- tribble(
  ~DataID, ~variable,
  12, "SLA",
  64, "SLA",  # This is marked as LMA, but based on units, is actually SLA
  13, "leaf_lifespan",
  15, "Nmass",
  65, "Narea",
  16, "Pmass",
  66, "Parea"
)

data_raw <- load_trait(data_ids)

trait_names <- unique(data_ids$variable)

data_raw %>%
  summarize_at(trait_names, ~quantile(., 0.995, na.rm = TRUE)) %>%
  glimpse()

sla_unit <- as_units("mm2 mg-1")
mass_unit <- as_units("mg g-1")
area_unit <- as_units("g m-2")
ll_unit <- as_units("months")

data_final <- data_raw %>%
  mutate(
    # Remove crazy values
    SLA = censor(SLA, SLA > 100 | SLA <= 0.2),
    Nmass = censor(Nmass, Nmass > 100 | Nmass <= 1e-1),
    Narea = censor(Narea, Narea > 20 | Narea <= 0.1),
    Pmass = censor(Pmass, Pmass > 10 | Pmass <= 0),
    Parea = censor(Parea, Parea > 1 | Parea <= 0.0014),
    leaf_lifespan = censor(leaf_lifespan, leaf_lifespan < 200),
    # Assign units
    leaf_lifespan = leaf_lifespan * ll_unit,
    SLA = SLA * sla_unit,
    Nmass = Nmass * mass_unit,
    Narea = Narea * area_unit,
    Pmass = Pmass * mass_unit,
    Parea = Parea * area_unit
  ) %>%
  filter_at(trait_names, any_vars(!is.na(.)))

diagnose_save(data_final, trait_names, "N_P_LL_SLA")
