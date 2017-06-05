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
