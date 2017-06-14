source('common.R')

# Data IDs:
#   - 13 -- leaf_lifespan
#   - 12 -- specific leaf area (SLA) -- 1/LMA
#   - 64 -- leaf mass per area (LMA) (NOTE: Based on units, this is stil SLA)
#   - 15 -- Nmass
#   - 65 -- Narea
#   - 16 -- Pmass
#   - 66 -- Parea
#   - 71 -- Rdmass  (at 25 C)
#   - 549 -- Vcmax_mass (at 25 C)
#   - 550 -- Vcmax_area (at 25 C)
#   - 2368 -- Rdarea

# Other information:
#   - 59 -- Latitude
#   - 60 -- Longitude

data_ids <- c("leaf_lifespan" = 13,
              "SLA" = 12,
              "LMA" = 64,
              "Nmass" = 15,
              "Narea" = 65,
              "Pmass" = 16,
              "Parea" = 66,
              #"Aarea" = 2356,
              #"Gs" = NA,       ## Not sure about this; stomatal conductance
              "Rdmass" = 46,
              "Rdmass" = 70,
              "Rdmass" = 71,
              "Rdmass" = 72,
              "Rdmass" = 1453,
              "Rdarea" = 69,
              "Rdarea" = 1456,
              "Rdarea" = 2285,
              "Rdarea" = 2368,
              "Rdarea" = 2375,
              "Rdarea" = 2381,
              "Vcmax_area" = 549,
              "Vcmax_mass" = 550,
              "Jmax_mass" = 664,
              "Jmax_mass" = 2369,
              "Jmax_mass" = 2383,
              "Jmax_area" = 665,
              "Jmax_area" = 1000,
              "Jmax_area" = 1119,
              "Jmax_area" = 2351,
              "Jmax_area" = 2372,
              "Jmax_area" = 2379,
              "Latitude" = 59,
              "Longitude" = 60,
              "Temperature_measurement" = 51,
              "Temperature_measurement" = 1666
              )
data_ids_noname <- unname(data_ids)

traits_long <- trydat %>%
    filter(DataID %in% data_ids_noname) %>% 
    mutate(StdValue = if_else(DataID %in% c(51, 1666), as.numeric(OrigValueStr), StdValue)) %>% 
    select(ObservationID, AccSpeciesID, DataID, StdValue, UnitName, ReferenceID) %>% 
    collect(n = Inf)

traits_proc <- traits_long %>% 
    filter(!is.na(StdValue)) %>% 
    mutate(trait = names(data_ids[match(DataID, data_ids)]) %>% factor)

references <- traits_proc %>% 
    count(ReferenceID, sort = TRUE)

write_csv(references, 'references.csv')

# Print units
distinct(traits_proc, trait, UnitName) # %>% write_csv('trait_units.csv')

traits_wide <- traits_proc %>% 
    select(ObservationID, AccSpeciesID, trait, StdValue) %>% 
    # Aggregate observations by mean
    group_by(ObservationID, AccSpeciesID, trait) %>% 
    summarize(value = mean(StdValue, na.rm = TRUE)) %>% 
    ungroup() %>% 
    spread(trait, value)

# Perform trait conversions
traits_fill <- traits_wide %>% 
    mutate(SLA = case_when(!is.na(.$SLA) ~ .$SLA,
                           !is.na(.$LMA) ~ .$LMA,
                           TRUE ~ NA_real_),
           LMA = 1/SLA) %>% 
    mutate(Nmass = case_when(!is.na(.$Nmass) ~ .$Nmass,
                             is.na(.$Nmass) & !is.na(.$Narea) & !is.na(.$SLA) ~ .$Narea * .$SLA,
                             TRUE ~ NA_real_)) %>% 
    mutate(Narea = case_when(!is.na(.$Narea) ~ .$Narea,
                             is.na(.$Narea) & !is.na(.$Nmass) & !is.na(.$LMA) ~ .$Nmass * .$LMA,
                             TRUE ~ NA_real_)) %>% 
    mutate(Pmass = case_when(!is.na(.$Pmass) ~ .$Pmass,
                             is.na(.$Pmass) & !is.na(.$Parea) & !is.na(.$SLA) ~ .$Parea * .$SLA,
                             TRUE ~ NA_real_)) %>% 
    mutate(Parea = case_when(!is.na(.$Parea) ~ .$Parea,
                             is.na(.$Parea) & !is.na(.$Pmass) & !is.na(.$LMA) ~ .$Pmass * .$LMA,
                             TRUE ~ NA_real_)) %>% 
    mutate(Rdmass = case_when(!is.na(.$Rdmass) ~ .$Rdmass,
                             is.na(.$Rdmass) & !is.na(.$Rdarea) & !is.na(.$SLA) ~ .$Rdarea * .$SLA,
                             TRUE ~ NA_real_)) %>% 
    mutate(Rdarea = case_when(!is.na(.$Rdarea) ~ .$Rdarea,
                             is.na(.$Rdarea) & !is.na(.$Rdmass) & !is.na(.$LMA) ~ .$Rdmass * .$LMA,
                             TRUE ~ NA_real_)) %>% 
    mutate(Vcmax_mass = case_when(!is.na(.$Vcmax_mass) ~ .$Vcmax_mass,
                             is.na(.$Vcmax_mass) & !is.na(.$Vcmax_area) & !is.na(.$SLA) ~ .$Vcmax_area * .$SLA,
                             TRUE ~ NA_real_)) %>% 
    mutate(Vcmax_area = case_when(!is.na(.$Vcmax_area) ~ .$Vcmax_area,
                             is.na(.$Vcmax_area) & !is.na(.$Vcmax_mass) & !is.na(.$LMA) ~ .$Vcmax_mass * .$LMA,
                             TRUE ~ NA_real_)) %>% 
    mutate(Jmax_mass = case_when(!is.na(.$Jmax_mass) ~ .$Jmax_mass,
                             is.na(.$Jmax_mass) & !is.na(.$Jmax_area) & !is.na(.$SLA) ~ .$Jmax_area * .$SLA,
                             TRUE ~ NA_real_)) %>% 
    mutate(Jmax_area = case_when(!is.na(.$Jmax_area) ~ .$Jmax_area,
                             is.na(.$Jmax_area) & !is.na(.$Jmax_mass) & !is.na(.$LMA) ~ .$Jmax_mass * .$LMA,
                             TRUE ~ NA_real_))

saveRDS(traits_fill, file = 'trait_data.rds')

############################################################

#try.cast <- dcast(try.sub, ObservationID + AccSpeciesID ~ DataName,
                  #value.var = "StdValue",
                  #fun.aggregate = mean, 
                  #na.rm=TRUE)

## Convert SLA to LMA
#try.cast[is.na(LMA) & !is.na(SLA), LMA := 1/SLA]
#try.cast[, SLA := NULL]

#try.cast[, c("log.LMA", "log.Nmass", "log.Pmass", "log.Rdmass", "log.LL") := lapply(.SD, log), .SDcols=c("LMA", "Nmass", "Pmass", "Rdmass", "leaf.lifespan")]

#saveRDS(try.cast, file = "try.subset.RData")

#%>%
    #filter(!(DatasetID == 50 & ValueKindName != "Single"),
           #!(DatasetID == 25 & ValueKindName != "Best estimate"),
           #!(DatasetID == 112 & ValueKindName != "Mean"),
           #!(DatasetID == 67 & ValueKindName != "Single"),
           #!(DatasetID == 158 & ValueKindName != "Single"),
           #!(DatasetID == 159 & ValueKindName != "Mean"),
           #!(DatasetID == 211 & ValueKindName != "Mean"),
           #!(DatasetID == 210 & ValueKindName != "Mean")) %>%
    #collect(n = Inf) %>%
    #setDT()
    ## 131 is fine as is
    ## 267 is fine as is

