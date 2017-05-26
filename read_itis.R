library(stringr)
library(magrittr)
library(tidyverse)

rawfile <- readLines('Plantae_Family.txt')[-(1:4)]
raw_trimmed <- str_trim(rawfile)
lead_spaces <- str_extract(rawfile, '^ +') %>% nchar()

tax_mat <- str_match(raw_trimmed, '^([A-z]+): +([A-z]+)$')[,-1]
colnames(tax_mat) <- c('level', 'value')

tax_dat <- as_data_frame(tax_mat) %>% 
    mutate(i = row_number(),
           spaces = lead_spaces)

tax_hier <- tax_dat %>% 
    distinct(level, spaces) %>% 
    arrange(spaces) %>% 
    .[['level']]

tax_wide <- tax_dat %>% 
    spread(level, value)

tax_filled <- tax_wide %>% 
    mutate(spaces_lead = lead(spaces, default = 0), spaces_lag = lag(spaces, default = 0)) %>% 
    select_(.dots = c('i', 'spaces', 'spaces_lead', 'spaces_lag', tax_hier)) %>% 
    mutate_if(is.character, (function(x) if_else(is.na(x) & .$spaces_lag > .$spaces, '---', x))) %>% 
    fill_(tax_hier[tax_hier != 'Family'])

tax_processed <- tax_filled %>% 
    filter(!is.na(Family), Family != '---') %>% 
    mutate_if(is.character, (function(x) if_else(is.na(x), 'n/a', x))) %>% 
    mutate_if(is.character, na_if, y = '---') %>% 
    fill_(tax_hier[tax_hier != 'Family']) %>% 
    select(-i, -spaces, -spaces_lead, -spaces_lag)

# Build SQLite phylogeny database

phylo_db <- src_sqlite('itis_taxonomy.sqlite', create = TRUE)

tax_processed %>% distinct(Superorder, Order, Suborder, Family) %>% 
    DBI::dbWriteTable(phylo_db$con, name = 'order_family', value = .)
tax_processed %>% distinct(Class, Subclass, Order) %>% 
    DBI::dbWriteTable(phylo_db$con, name = 'class_order', value = .)
tax_processed %>% distinct(Superdivision, Division, Subdivision, Class) %>% 
    DBI::dbWriteTable(phylo_db$con, name = 'division_class', value = .)
tax_processed %>% distinct(Kingdom, Subkingdom, Infrakingdom, Division) %>% 
    DBI::dbWriteTable(phylo_db$con, name = 'kingdom_division', value = .)

#write_csv(tax_processed, 'itis_taxonomy.csv')

