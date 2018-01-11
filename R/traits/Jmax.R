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

jmax_raw <- load_trait(jmax_id)

# Get leaf temperature
jmax_rawtemp <- jmax_raw %>%
  left_join(get_leaf_temp()) %>%
  fill_reference_temp()

summarize_at(jmax_rawtemp, vars(starts_with("jmax")), missfuns) %>% glimpse()

if (FALSE) {
  message("NOTE: Missing temperature data for the following references.")
  jmax_rawtemp %>%
    filter_at(vars(matches("_temperature")), all_vars(is.na(.))) %>%
    count(ReferenceID, sort = TRUE)
}

jmax_temp_scale <- function(jmax_t, t_meas) {
  ps_temp_scale(jmax_t, t_meas, H_a_jmax, dS_jmax)
}

area_unit <- as_units("micromol m-2 s-1")
mass_unit <- as_units("micromol g-1 s-1")

jmax_names <- c("Jmax_mass", "Jmax_area")

jmax_final <- jmax_rawtemp %>%
  mutate(
    Jmax_area = case_when(
      !is.na(Jmax_area_25) ~ Jmax_area_25,
      !is.na(Jmax_area_ambient) & !is.na(measurement_temperature) ~
        jmax_temp_scale(Jmax_area_ambient, measurement_temperature),
      TRUE ~ NA_real_
    ),
    Jmax_mass = case_when(
      !is.na(Jmax_mass_25) ~ Jmax_mass_25,
      !is.na(Jmax_mass_ambient) & !is.na(measurement_temperature) ~
        jmax_temp_scale(Jmax_mass_ambient, measurement_temperature),
      TRUE ~ NA_real_
    ),
    Jmax_area = if_else(Jmax_area <= 0, NA_real_, Jmax_area),
    Jmax_mass = if_else(Jmax_mass <= 0, NA_real_, Jmax_mass),
    Jmax_area = Jmax_area * area_unit,
    Jmax_mass = Jmax_mass * mass_unit,
  ) %>%
  select(ObservationID, Jmax_area, Jmax_mass, ReferenceID) %>%
  filter_at(jmax_names, any_vars(!is.na(.)))

diagnose_save(jmax_final, jmax_names, "Jmax")

if (FALSE) {
  jmax_final %>%
    left_join(jmax_rawtemp) %>%
    ggplot() +
    aes(x = as.numeric(Jmax_area), y = Jmax_area_ambient, color = factor(ReferenceID)) +
    geom_point() +
    geom_abline(linetype = "dashed")
}
