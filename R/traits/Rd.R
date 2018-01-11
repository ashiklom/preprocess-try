source(here::here("R", "common.R"))

# Extract Rdarea and Rdmass
# Data IDs:
# - 46 -- Rdmass, at leaf temperature (?)
# - 69 -- Rdarea, at leaf temperature
# - 70 -- Rdmass, at leaf temperature
# - 71 -- Rdmass, at 25C
# - 72 -- Rdmass, at 25C (measured)
rd_ids <- c(69, 70, 71, 72)
rd_raw <- trydat %>%
  filter(DataID %in% rd_ids) %>%
  select(ObservationID, DataID, ReferenceID, value = StdValue, UnitName) %>%
  collect() %>%
  mutate(
    variable = recode_factor(
      DataID,
      `69` = "Rd_area_LT",
      `70` = "Rd_mass_LT",
      `71` = "Rd_mass_25",
      `72` = "Rd_mass_25m"
    )
  ) %>%
  select(-DataID) %>%
  spread(variable, value)

# Get leaf temperature
rd_rawtemp <- rd_raw %>%
  left_join(get_leaf_temp()) %>%
  fill_reference_temp()

message("NOTE: Missing temperature data for the following references.")
rd_rawtemp %>%
  filter_at(vars(matches("_temperature")), all_vars(is.na(.))) %>%
  count(ReferenceID, sort = TRUE)

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
    )
  ) %>%
  select(ObservationID, Rdmass, Rdarea, ReferenceID) %>%
  filter(
    !(is.na(Rdmass) & is.na(Rdarea)),
    !(!is.na(Rdmass) & Rdmass <= 0),
    !(!is.na(Rdarea) & Rdarea <= 0)
  )

rd_final <- rd_proc %>%
  mutate(
    Rdmass = as_units(Rdmass, "micromol g-1 s-1") %>%
      set_units("nanomol g-1 s-1"),
    Rdarea = as_units(Rdarea, "micromol m-2 s-1")
  )

diag_plot <- rd_final %>%
  gather("trait", "value", Rdmass, Rdarea, na.rm = TRUE) %>%
  ggplot() +
  aes(x = factor(ReferenceID), y = value) +
  geom_jitter(size = 0.1) +
  facet_wrap(~ trait, scales = "free")
if (interactive()) diag_plot
ggsave("diagnostics/Rd.pdf", diag_plot)

write_rds(rd_final, "processed/traits/Rd.rds")
