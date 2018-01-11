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

data_raw <- trydat %>%
  filter(DataID %in% data_ids$DataID) %>%
  select(ObservationID, DataID, ReferenceID, value = StdValue, UnitName) %>%
  collect()

data_raw2 <- data_raw %>%
  filter(!is.na(value)) %>%
  mutate(
    DataID = factor(DataID, data_ids$DataID),
    variable = lvls_revalue(DataID, data_ids$variable)
  ) %>%
  group_by(variable, ObservationID, ReferenceID) %>%
  arrange(DataID) %>%
  summarize(value = value[1]) %>%
  ungroup() %>%
  spread(variable, value)

trait_names <- unique(data_ids$variable)

data_raw2 %>%
  summarize_at(trait_names, ~quantile(., 0.995, na.rm = TRUE)) %>%
  glimpse()

sla_unit <- make_units(mm ^ 2 * mg ^ -1)
mass_unit <- make_units(mg * g ^ -1)
area_unit <- make_units(g * m ^ -2)
ll_unit <- make_units(months)

data_final <- data_raw2 %>%
  mutate(
    leaf_lifespan = leaf_lifespan * ll_unit,
    SLA = SLA * sla_unit,
    Nmass = Nmass * mass_unit,
    Narea = Narea * area_unit,
    Pmass = Pmass * mass_unit,
    Parea = Parea * area_unit
  ) %>%
  # Remove crazy values
  filter_at(trait_names, any_vars(!is.na(.))) %>%
  filter(
    SLA < 100 * sla_unit,
    Nmass < 200 * mass_unit,
    Narea < 20 * area_unit,
    Pmass < 10 * mass_unit,
    Parea < 1 * area_unit
  )

diag_plot <- data_final %>%
  gather("trait", "value", trait_names, na.rm = TRUE) %>%
  ggplot() +
  aes(x = factor(ReferenceID), y = value) +
  geom_jitter(size = 0.1) +
  facet_wrap(~ trait, scales = "free")
if (interactive()) diag_plot
ggsave("diagnostics/N_P_LL_SLA.pdf", diag_plot)

write_rds(data_final, "processed/traits/N_P_LL_SLA.R")
