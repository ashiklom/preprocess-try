library(tidyverse)
trydb <- src_sqlite('try.sqlite')
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

