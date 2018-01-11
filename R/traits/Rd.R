source(here::here("R", "common.R"))

# Extract Rdarea and Rdmass
# - LT -- At leaf temperature
# - 25 -- At 25 C
data_ids <- tribble(
  ~DataID, ~variable,
  69, "Rd_area_LT",
  71, "Rd_mass_25",
  72, "Rd_mass_25m",
  46, "Rd_mass_LT",
  70, "Rd_mass_LT"
)

data_raw <- load_trait(data_ids)

# Get leaf temperature
rd_rawtemp <- data_raw %>%
  left_join(get_leaf_temp()) %>%
  fill_reference_temp()

if (FALSE) {
  message("NOTE: Missing temperature data for the following references.")
  rd_rawtemp %>%
    filter_at(vars(matches("_temperature")), all_vars(is.na(.))) %>%
    count(ReferenceID, sort = TRUE)
}

# Same as Atkin et al. 2015
rd_temp_correction <- function(Rdmeas, Tmeas, Tref = 25) {
  base <- 3.09 - 0.043 * ((Tref + Tmeas) / 2)
  powr <- (Tref - Tmeas) / 10
  Rdmeas * base ^ powr
}

rd_proc <- rd_rawtemp %>%
  mutate(
    Rdarea = case_when(
      !is.na(Rd_area_LT) & !is.na(respiration_temperature) ~
        rd_temp_correction(Rd_area_LT, respiration_temperature),
      !is.na(Rd_area_LT) & !is.na(measurement_temperature) ~
        rd_temp_correction(Rd_area_LT, measurement_temperature),
      TRUE ~ NA_real_
    ),
    Rdmass = case_when(
      !is.na(Rd_mass_25) ~ Rd_mass_25,
      !is.na(Rd_mass_25m) ~ Rd_mass_25m,
      !is.na(Rd_mass_LT) & !is.na(respiration_temperature) ~
        rd_temp_correction(Rd_mass_LT, respiration_temperature),
      !is.na(Rd_mass_LT) & !is.na(measurement_temperature) ~
        rd_temp_correction(Rd_mass_LT, measurement_temperature),
      TRUE ~ NA_real_
    ),
    # Se
    Rdarea = if_else(Rdarea <= 0, NA_real_, Rdarea),
    Rdmass = if_else(Rdmass <= 0, NA_real_, Rdmass)
  ) %>%
  select(ObservationID, Rdmass, Rdarea, ReferenceID) %>%
  filter_at(c("Rdmass", "Rdarea"), any_vars(!is.na(.)))

rd_final <- rd_proc %>%
  mutate(
    Rdmass = as_units(Rdmass, "micromol g-1 s-1") %>%
      set_units("nanomol g-1 s-1"),
    Rdarea = as_units(Rdarea, "micromol m-2 s-1")
  )

diagnose_save(rd_final, c("Rdmass", "Rdarea"), "Rd")
