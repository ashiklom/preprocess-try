library(tidyverse)

trydb <- src_sqlite('try.sqlite')

species_merge <- readRDS('pfts_species/tps_species.rds')

families <- species_merge %>% 
    filter(!is.na(Family)) %>% 
    distinct(Family)

tol_fname <- 'pfts_species/tol_classification.rds'
tol_present <- file.exists(tol_fname)

if (tol_present) {
    old_tol <- readRDS(tol_fname)
    missing_families <- anti_join(families, old_tol)
} else {
    missing_families <- families
}

new_families <- nrow(missing_families) > 0

if (new_families) {
    mf_famid <- mutate(missing_families, tol_id = taxize::get_tolid(Family))
    mf_rawclass <- mutate(mf_famid, classification = taxize::classification(tol_id))
    mf_class <- mutate(mf_rawclass, classification = map(classification, as_data_frame))
}

if (tol_present && new_families) {
    fam_dat <- full_join(old_tol, mf_class)
} else if (tol_present && !new_families) { 
    fam_dat <- old_tol
} else {
    fam_dat <- mf_class
}

saveRDS(fam_dat, tol_fname)

get_rank <- function(dat, rank) {
    ranknames <- dat[['rank']]
    rankvals <- dat[['name']]
    ind <- which(ranknames == rank)
    if (length(ind) == 0) {
        return(NA)
    } else if (length(ind) == 1) {
        return(rankvals[ind])
    } else {
        stop('Found ', length(ind), ' ranks called "', rank, '".')
    }
}

find_rank <- function(dat, pattern) {
    ind <- grep(pattern, dat$name)
    if (length(ind) == 0) {
        return(NA)
    } else if (length(ind) == 1) {
        return(dat[['name']][ind])
    } else {
        stop('Found ', length(ind), ' names matching pattern "', pattern, '".')
    }
}

fam_processed <- fam_dat %>% 
    mutate(
           tol_Family = map_chr(classification, get_rank, rank = 'family'),
           Order = map_chr(classification, get_rank, rank = 'order'),
           SubClass = map_chr(classification, get_rank, rank = 'subclass'),
           Class = map_chr(classification, get_rank, rank = 'class'),
           PlantGroup = map_chr(classification, find_rank, pattern = 'gymno|angio')
           )

family_taxonomy <- select(fam_processed, -classification)

#family_taxonomy %>% filter(Family != tol_Family)

species_tax <- left_join(species_merge, family_taxonomy)

species_tax %>% 
    filter(is.na(tol_Family)) %>% 
    arrange(desc(N))

#species_tax %>% 
    #count(AccSpeciesName) %>% 
    #filter(n > 1)

# Write to new table in TryDB 
if (db_has_table(trydb$con, 'species_phylo')) {
    DBI::dbSendQuery(trydb$con, 'DROP TABLE species_phylo')
}
db_insert_into(trydb$con, 'species_phylo', species_tax)
file.create('.family', showWarnings = FALSE)

