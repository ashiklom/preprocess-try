source(here::here("R", "common.R"))

data_ids <- tribble(
  ~DataID, ~variable,
  59, "Latitude",
  60, "Longitude"
)

data_raw <- load_trait(data_ids) %>%
  select(-ReferenceID) %>%
  distinct()
write_rds(data_raw, "processed/pfts/lat_lon.rds")
