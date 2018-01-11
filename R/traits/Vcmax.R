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

vcmax_raw <- load_trait(vcmax_id)

# Get leaf temperature
vcmax_rawtemp <- vcmax_raw %>%
  left_join(get_leaf_temp()) %>%
  fill_reference_temp()

summarize_at(vcmax_rawtemp, vars(starts_with("Vcmax")), missfuns) %>% glimpse()

if (FALSE) {
  message("NOTE: Missing temperature data for the following references.")
  vcmax_rawtemp %>%
    filter_at(vars(matches("_temperature")), all_vars(is.na(.))) %>%
    count(ReferenceID, sort = TRUE)
}

# Vcmax temeprature correction is hard, so not bothering with it
vcmax_names <- c("Vcmax_mass", "Vcmax_area")
vcmax_final <- vcmax_rawtemp %>%
  select(
    ObservationID,
    Vcmax_area = Vcmax_area_25,
    Vcmax_mass = Vcmax_mass_25,
    ReferenceID
  ) %>%
  mutate_at(vcmax_names, ~ if_else(. <= 0, NA_real_, .)) %>%
  filter_at(vcmax_names, any_vars(!is.na(.))) %>%
  mutate(
    Vcmax_area = set_units(Vcmax_area, "micromol m-2 s-1"),
    Vcmax_mass = set_units(Vcmax_mass, "micromol g-1 s-1")
  )

diagnose_save(vcmax_final, vcmax_names, "Vcmax")
