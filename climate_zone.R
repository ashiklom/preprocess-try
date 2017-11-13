source("common.R")

trait_data <- readRDS("traits/traits_with_climate.rds")

species_climate <- trait_data %>%
    group_by(AccSpeciesID) %>%
    summarize(mean_AMT = mean(AMT, na.rm = TRUE))

def_climate_zone <- function(mean_AMT) {
    #result <- numeric(length(mean_AMT))
    #is_missing <- is.na(mean_AMT)
    #present <- result[!is_missing]
    #result[is_missing] <- NA
    climate_zone <- case_when(
        mean_AMT > 20 ~ "tropical",
        mean_AMT > 5 ~ "temperate",
        !is.na(mean_AMT) ~ "boreal",
        TRUE ~ NA_character_
    )
}

# Use the following code for creating an IRIDL map of this:
# SOURCES .NOAA .NCEP .CPC .GHCN_CAMS .gridded .deg0p5 .temp yearlyAverage
#   [T]average
#   {boreal 278.15 temperate 293.15 tropical} (climatezone) classify
#   a:
#     climatezone (boreal) VALUE
#     :a:
#      climatezone (temperate) VALUE
#      2. mul
#      :a:
#       climatezone (tropical) VALUE
#       3. mul
#       :a
#     add
#    add
#   startcolormap
#     /maxncolor 7 def
#     DATA
#     0.5 3.5 RANGE
#     transparent blue blue red green green endcolormap

cz1 <- species_climate %>%
    mutate(climate_zone = def_climate_zone(mean_AMT))

cz1 %>% count(climate_zone, sort = TRUE)

#species_missing_climate <- species_climate %>% 
    #filter(is.na(mean_AMT)) %>% 
    #distinct(AccSpeciesID)

cz_final <- filter(cz1, !is.na(climate_zone))

saveRDS(cz1, "attributes/climate_zone.rds")
