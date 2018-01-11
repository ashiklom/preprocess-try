source(here::here("R", "common.R"))

# Extract Jmax_mass and Jmax_area
# - LT = Leaf temperature
# - 25 = 25 degrees C
# - unk = Unknown (assumed at leaf temperature)
# Arranged in order of trustworthiness (high to low)
jmax_id <- tribble(
  ~DataID, ~variable,
  2351, "Jmax_area_25",
  2369, "Jmax_mass_25",
  2379, "Jmax_area_25",
  2383, "Jmax_mass_25",
  664, "Jmax_mass_ambient",
  665, "Jmax_area_ambient",
  2372, "Jmax_area_unk"
)

jmax_raw <- trydat %>%
  filter(DataID %in% jmax_id$DataID) %>%
  select(ObservationID, DataID, ReferenceID, value = StdValue, UnitName) %>%
  collect()

message("Jmax units:")
jmax_raw %>%
  distinct(UnitName) %>%
  print()

jmax_raw2 <- jmax_raw %>%
  filter(!is.na(value)) %>%
  mutate(
    DataID = factor(DataID, jmax_id$DataID),
    variable = lvls_revalue(DataID, jmax_id$variable)
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
jmax_rawtemp <- jmax_raw2 %>%
  left_join(get_leaf_temp()) %>%
  fill_reference_temp()

message("NOTE: Missing temperature data for the following references.")
jmax_rawtemp %>%
  filter_at(vars(matches("_temperature")), all_vars(is.na(.))) %>%
  count(ReferenceID, sort = TRUE)

n_area_25 <- jmax_rawtemp %>%
  filter(!is.na(Jmax_area_25)) %>%
  count() %>%
  pull(n)

n_area_other <- jmax_rawtemp %>%
  filter(is.na(Jmax_area_25), !is.na(Jmax_area_ambient) || !is.na(Jmax_area_unk)) %>%
  count() %>%
  pull(n)

n_area_total <- jmax_rawtemp %>%
  filter_at(vars(starts_with("Jmax_area")), any_vars(!is.na(.))) %>%
  count() %>%
  pull(n)

n_mass_25 <- jmax_rawtemp %>%
  filter(!is.na(Jmax_mass_25)) %>%
  count() %>%
  pull(n)

message("Jmax summary:")
message(n_area_25, " measurements of Jmax_area already at 25 C.")
message(n_area_other, " of ", n_area_total,
        " (", format(100 * n_area_other / n_area_total, digits = 1), "%)",
        " measurements of Jmax_area ",
        "not at 25 C.")
message(n_mass_25, " measurements of Jmax_mass already at 25 C.")

jmax_temp_scale <- function(jmax_t, t_meas) {
  ps_temp_scale(jmax_t, t_meas, H_a_jmax, dS_jmax)
}

jmax_final <- jmax_rawtemp %>%
  mutate(
    Jmax_area = case_when(
      !is.na(Jmax_area_25) ~ Jmax_area_25,
      !is.na(Jmax_area_ambient) & !is.na(measurement_temperature) ~
        jmax_temp_scale(Jmax_area_ambient, measurement_temperature),
      TRUE ~ NA_real_
    ),
    Jmax_area = as_units(Jmax_area, "micromol m-2 s-1"),
    Jmax_mass = case_when(
      !is.na(Jmax_mass_25) ~ Jmax_mass_25,
      !is.na(Jmax_mass_ambient) & !is.na(measurement_temperature) ~
        jmax_temp_scale(Jmax_mass_ambient, measurement_temperature),
      TRUE ~ NA_real_
    ),
    Jmax_mass = as_units(Jmax_mass, "micromol g-1 s-1")
  ) %>%
  select(ObservationID, Jmax_area, Jmax_mass, ReferenceID) %>%
  filter(
    !is.na(Jmax_area) | !is.na(Jmax_mass),
    !(as.numeric(Jmax_area) <= 0),
    !(as.numeric(Jmax_mass) <= 0)
  )

diag_plot <- jmax_final %>%
  gather("trait", "value", Jmax_mass, Jmax_area, na.rm = TRUE) %>%
  ggplot() +
  aes(x = factor(ReferenceID), y = value) +
  geom_jitter(size = 0.1) +
  facet_wrap(~ trait, scales = "free")
if (interactive()) diag_plot
ggsave("diagnostics/Jmax.pdf", diag_plot)

if (FALSE) {
  jmax_final %>%
    left_join(jmax_rawtemp) %>%
    ggplot() +
    aes(x = as.numeric(Jmax_area), y = Jmax_area_ambient, color = factor(ReferenceID)) +
    geom_point() +
    geom_abline(linetype = "dashed")
}

write_rds(jmax_final, "processed/traits/Jmax.rds")
