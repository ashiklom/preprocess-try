source('common.R')

# Data IDs:
#   - 13 -- leaf_lifespan
#   - 12 -- specific leaf area (SLA) -- 1/LMA
#   - 15 -- Nmass
#   - 65 -- Narea
#   - 16 -- Pmass
#   - 66 -- Parea
#   - 71 -- Rdmass

#   - 2356 -- Aarea
#   - 2368 -- Rdarea

# Other traits:
#   - 64 -- leaf mass per area (LMA) -- 1/SLA

data_ids <- c("leaf_lifespan" = 13,
              "SLA" = 12,
              "LMA" = 64,
              "Nmass" = 15,
              "Narea" = 65,
              "Pmass" = 16,
              "Parea" = 66,
              #"Aarea" = 2356,  ## Not sure about this
              #"Amass" = NA,    ## Couldn't find this
              #"Gs" = NA,       ## Not sure about this; stomatal conductance
              "Rdmass" = 71
              #"Rdarea" = 2368   ## Exclude for now
              )
data_ids_noname <- unname(data_ids)

traits_long <- trydat %>%
    filter(DataID %in% data_ids_noname) %>% 
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
    mutate(LMA = case_when(!is.na(.$LMA) ~ .$LMA,
                           !is.na(.$SLA) ~ 1/.$SLA,
                           TRUE ~ NA_real_
                           ),
           SLA = case_when(!is.na(.$SLA) ~ .$SLA,
                           !is.na(.$LMA) ~ 1/.$LMA,
                           TRUE ~ NA_real_))

pfts <- read_csv('try_pfts.csv')

traits_pfts <- traits_fill %>% 
    left_join(pfts)

traits_pfts %>% 
    count(pft, sort = TRUE)

traits_pfts %>% 
    group_by(pft) %>% 
    summarize_at(vars(-ObservationID, -AccSpeciesID),
                 (function(x) sum(!is.na(x))))

saveRDS(traits_data, file = 'trait_data.RData')

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

