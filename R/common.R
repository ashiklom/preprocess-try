library(tidyverse)
library(units)

trydb <- src_sqlite(here::here("try.sqlite"))
species <- tbl(trydb, 'orig_species')
datanames <- tbl(trydb, 'orig_datanames')
trydat <- tbl(trydb, 'orig_data')
refs <- tbl(trydb, 'orig_citations')

check_unique_species <- function(dat) {
    stopifnot((dat %>% count(AccSpeciesID) %>% filter(n > 1) %>% nrow()) == 0)
}

raw_string <- function(data_long, sep = ' ', lower = TRUE) {
    data_long %>% 
        select(AccSpeciesID, OrigValueStr) %>% 
        group_by(AccSpeciesID) %>% 
        summarize(rawstring = paste(unique(OrigValueStr), collapse = sep) %>% 
                  (function(.) if (lower) tolower(.) else (.)) %>% 
                  trimws)
}

lookup <- function(did, rid, string, data, fname, column) {
    dat_in <- read_csv(fname)
    data %>% 
        filter(DataID == did, ReferenceID == rid, OrigValueStr == string) %>% 
        distinct(AccSpeciesID) %>% 
        left_join(data %>% left_join(dat_in)) %>% 
        count_(column, sort = TRUE)
}

prep_sheet <- . %>%
    count(DataID, DataName, ReferenceID, OrigValueStr, sort = TRUE) %>% 
    ungroup() %>% 
    left_join(datanames %>% collect()) %>% 
    select(DataID, DataName, ReferenceID, OrigValueStr, n)

most_frequent <- function(val, tie = NA) {
    val_class <- class(val)
    val_counts <- table(val)
    max_ind <- which(val_counts == max(val_counts))
    if (length(max_ind) > 1) {
        if (!is.na(tie)) {
            tie_list <- seq_along(tie)
            names(tie_list) <- tie
            out_full <- tie_list[val[max_ind]]
            out <- names(sort(out_full))[1]
        } else {
            out <- paste(sort(val[max_ind]), collapse = '|')
        }
    } else {
        out <- val[max_ind]
    }
    return(out)
}

get_leaf_temp <- function() {
  temp_ids <- c(
    1666,   # Leaf temperature (deg C)
    322,    # Temperature during measurement (deg C)
    51,     # Temperature during respiration measurement (deg C)
    339,    # Temperature during measurement (deg C)
    243     # Exposition temperature (deg C)
  )
  # DataID 1666 -- leaf temperature (degrees C)
  temp_raw <- trydat %>%
    filter(DataID %in% temp_ids) %>%
    select(ObservationID, DataID, value = OrigValueStr) %>%
    collect() %>%
    mutate(
      variable = recode(
        DataID,
        `51` = "respiration_temperature",
        `322` = "measurement_temperature",
        `339` = "measurement2_temperature",
        `243` = "exposition_temperature",
        `1666` = "leaf_temperature"
      ),
      value = as.numeric(value)
    ) %>%
    select(-DataID)

  # Only one measurement temperature should exist
  # (Exclude measurement-specific temperatures -- I always give those priority)
  special_temp <- temp_raw %>%
    filter(variable %in% c("respiration_temperature"))
  meas_raw <- temp_raw %>%
    anti_join(special_temp, by = "ObservationID")
  duplicated_meas <- meas_raw %>%
    count(ObservationID) %>%
    filter(n > 1)
  if (nrow(duplicated_meas) > 0) {
    meas_raw %>%
      semi_join(duplicated_meas) %>%
      arrange(ObservationID) %>%
      print()
    stop("Found duplicated temperature measurements")
  }
  meas_temp <- meas_raw %>%
    group_by(ObservationID) %>%
    summarize(
      variable = "measurement_temperature",
      value = unique(value)
    )
  bind_rows(special_temp, meas_temp) %>%
    spread(variable, value)
}

# Reference-specific measurement temperatures (deg C), based on literature
fill_reference_temp <- function(dat) {
  dat %>%
    mutate(
      measurement_temperature = case_when(
        !is.na(measurement_temperature) ~ measurement_temperature,
        ReferenceID == 146 ~ 30,    # All measurements at this temperature
        ReferenceID == 181 ~ 30.9,
        TRUE ~ NA_real_
      )
    )
}
