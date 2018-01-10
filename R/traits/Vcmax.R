source(here::here("R", "common.R"))

# Extract Vcmax_mass and Vcmax_area
# - LT = Leaf temperature
# - 25 = 25 degrees C
# - unk = Unknown (assumed at leaf temperature)
# Arranged in order of trustworthiness (high to low)
vcmax_id <- tribble(
  ~DataID, ~variable,
  550, "Vcmax_area_25",
  549, "Vcmax_mass_25",
  547, "Vcmax_mass_LT",
  2378, "Vcmax_area_25",
  2382, "Vcmax_mass_25",
  1275, "Vcmax_area_LT",
  1232, "Vcmax_area_unk",
  2371, "Vcmax_area_unk"
)

vcmax_raw <- trydat %>%
  filter(DataID %in% vcmax_id$DataID) %>%
  select(ObservationID, DataID, ReferenceID, value = StdValue, UnitName) %>%
  collect()

message("Vcmax units:")
vcmax_raw %>%
  distinct(UnitName) %>%
  print()

vcmax_raw2 <- vcmax_raw %>%
  filter(!is.na(value)) %>%
  mutate(
    DataID = factor(DataID, vcmax_id$DataID),
    variable = lvls_revalue(DataID, vcmax_id$variable)
  ) %>%
  # Deduplicate:
  # Sort by trustworthiness within each trait (DataID factor levels), then take
  # the most trustworthy value.
  group_by(variable, ObservationID, ReferenceID) %>%
  arrange(DataID) %>%
  summarize(value = value[1]) %>%
  ungroup() %>%
  spread(variable, value)

# Get leaf temperature
vcmax_rawtemp <- vcmax_raw2 %>%
  left_join(get_leaf_temp()) %>%
  fill_reference_temp()

message("NOTE: Missing temperature data for the following references.")
vcmax_rawtemp %>%
  filter_at(vars(matches("_temperature")), all_vars(is.na(.))) %>%
  count(ReferenceID, sort = TRUE)

n_area_25 <- vcmax_rawtemp %>%
  filter(!is.na(Vcmax_area_25)) %>%
  count() %>%
  pull(n)

n_area_other <- vcmax_rawtemp %>%
  filter(is.na(Vcmax_area_25), !is.na(Vcmax_area_LT) || !is.na(Vcmax_area_unk)) %>%
  count() %>%
  pull(n)

n_area_total <- vcmax_rawtemp %>%
  filter_at(vars(starts_with("Vcmax_area")), any_vars(!is.na(.))) %>%
  count() %>%
  pull(n)

n_mass_25 <- vcmax_rawtemp %>%
  filter(!is.na(Vcmax_mass_25)) %>%
  count() %>%
  pull(n)

message("Vcmax summary:")
message(n_area_25, " measurements of Vcmax_area already at 25 C.")
message(n_area_other, " of ", n_area_total,
        " (", format(100 * n_area_other / n_area_total, digits = 1), "%)",
        " measurements of Vcmax_area ",
        "not at 25 C.")
message(n_mass_25, " measurements of Vcmax_mass already at 25 C.")

# Vcmax temeprature correction is hard, so not bothering with it
vcmax_final <- vcmax_rawtemp %>%
  select(
    ObservationID,
    Vcmax_area = Vcmax_area_25,
    Vcmax_mass = Vcmax_mass_25,
    ReferenceID
  ) %>%
  filter(
    !is.na(Vcmax_mass) | !is.na(Vcmax_area),
    !(Vcmax_mass <= 0),
    !(Vcmax_area <= 0) 
  ) %>%
  mutate(
    Vcmax_area = set_units(Vcmax_area, "micromol m-2 s-1"),
    Vcmax_mass = set_units(Vcmax_mass, "micromol g-1 s-1")
  )

diag_plot <- vcmax_final %>%
  gather("trait", "value", Vcmax_mass, Vcmax_area, na.rm = TRUE) %>%
  ggplot() +
  aes(x = factor(ReferenceID), y = value) +
  geom_jitter(size = 0.1) +
  facet_wrap(~ trait, scales = "free")
if (interactive()) diag_plot
ggsave(here("diagnostics", "Vcmax.pdf"), diag_plot)

saveRDS(vcmax_final, here("processed", "traits", "Vcmax.rds"))
