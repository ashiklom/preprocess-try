# JULES 1
jules1_levels <- c('broadleaf', 
                   'needleleaf', 
                   'shrub',
                   'c3_grass', 
                   'c4_grass')
jules1_assign <- function(growth_form, ps_pathway, leaf_type) {
    pft <- NA_character_
    if (isTRUE(growth_form == 'graminoid')) {
        if (isTRUE(ps_pathway == 'C4')) {
            pft <- 'c4_grass'
        } else {
            pft <- 'c3_grass'
        }
    } else if (isTRUE(growth_form == 'shrub')) {
        pft <- 'shrub'
    } else if (isTRUE(growth_form == 'tree')) {
        if (isTRUE(leaf_type == 'needle')) {
            pft <- 'needleleaf'
        } else if (isTRUE(leaf_type == 'broad')) {
            pft <- 'broadleaf'
        }
    }
    pft_factor <- factor(pft, levels = jules1_levels)
    return(pft_factor)
}

# JULES 2
jules2_levels <- c('broadleaf_evergreen_tropical', 
                   'broadleaf_evergreen_temperate', 
                   'broadleaf_deciduous',
                   'needleleaf_evergreen',
                   'needleleaf_deciduous',
                   'shrub_evergreen',
                   'shrub_deciduous',
                   'c3_grass', 
                   'c4_grass')
jules2_assign <- function(growth_form, ps_pathway, leaf_type, phenology, climate_zone) {
    pft <- NA_character_
    if (isTRUE(growth_form == 'graminoid')) {
        if (isTRUE(ps_pathway == 'C4')) {
            pft <- 'c4_grass'
        } else {
            pft <- 'c3_grass'
        }
    } else if (isTRUE(growth_form == 'shrub')) {
        if (isTRUE(phenology == 'deciduous')) {
            pft <- 'shrub_deciduous'
        } else if (isTRUE(phenology == 'evergreen')) {
            pft <- 'shrub_evergreen'
        }
    } else if (isTRUE(growth_form == 'tree')) {
        if (isTRUE(leaf_type == 'needle')) {
            if (isTRUE(phenology == 'deciduous')) {
                pft <- 'needleleaf_deciduous'
            } else {
                pft <- 'needleleaf_evergreen'
            }
        } else if (isTRUE(leaf_type == 'broad')) {
            if (isTRUE(phenology == 'deciduous')) {
                pft <- 'broadleaf_deciduous'
            } else if (isTRUE(phenology == 'evergreen')) {
                if (isTRUE(climate_zone == 'tropical')) {
                    pft <- 'broadleaf_evergreen_tropical'
                } else if (isTRUE(climate_zone %in% c('temperate', 'boreal'))) {
                    pft <- 'broadleaf_evergreen_temperate'
                }
            }
        }
    }
    pft_factor <- factor(pft, levels = jules2_levels)
    return(pft_factor)
}

# CLM 4.5
clm45_levels <- c('broadleaf_evergreen_tropical', 
                  'broadleaf_evergreen_temperate', 
                  'broadleaf_deciduous_tropical',
                  'broadleaf_deciduous_temperate',
                  'needleleaf_evergreen',
                  'needleleaf_deciduous',
                  'shrub_evergreen',
                  'shrub_deciduous_temperate',
                  'shrub_deciduous_boreal',
                  'c3_grass_arctic', 
                  'c3_grass_temperate',
                  'c4_grass')
clm45_assign <- function(growth_form, ps_pathway, leaf_type, phenology, climate_zone) {
    pft <- NA_character_
    if (isTRUE(growth_form == 'graminoid')) {
        if (isTRUE(ps_pathway == 'C4')) {
            pft <- 'c4_grass'
        } else if (isTRUE(climate_zone == 'boreal')) {
            pft <- 'c3_grass_arctic'
        } else {
            pft <- 'c3_grass_temperate'
        }
    } else if (isTRUE(growth_form == 'shrub')) {
        if (isTRUE(phenology == 'deciduous')) {
            if (isTRUE(climate_zone == 'boreal')) {
                pft <- 'shrub_deciduous_boreal'
            } else if (isTRUE(climate_zone == 'temperate')) {
                pft <- 'shrub_deciduous_temperate'
            }
        } else if (isTRUE(phenology == 'evergreen')) {
            pft <- 'shrub_evergreen'
        }
    } else if (isTRUE(growth_form == 'tree')) {
        if (isTRUE(leaf_type == 'needle')) {
            if (isTRUE(phenology == 'deciduous')) {
                pft <- 'needleleaf_deciduous'
            } else {
                pft <- 'needleleaf_evergreen'
            }
        } else if (isTRUE(leaf_type == 'broad')) {
            if (isTRUE(phenology == 'deciduous')) {
                if (isTRUE(climate_zone == 'tropical')) {
                    pft <- 'broadleaf_deciduous_tropical'
                } else if (isTRUE(climate_zone %in% c('temperate', 'boreal'))) {
                    pft <- 'broadleaf_deciduous_temperate'
                }
            } else if (isTRUE(phenology == 'evergreen')) {
                if (isTRUE(climate_zone == 'tropical')) {
                    pft <- 'broadleaf_evergreen_tropical'
                } else if (isTRUE(climate_zone %in% c('temperate', 'boreal'))) {
                    pft <- 'broadleaf_evergreen_temperate'
                }
            }
        }
    }
    pft_factor <- factor(pft, levels = clm45_levels)
    return(pft_factor)
}

# My custom PFT scheme
custom_levels <- c('broadleaf_evergreen_tropical',
                   'broadleaf_evergreen_temperate',
                   'broadleaf_deciduous_tropical',
                   'broadleaf_deciduous_temperate',
                   'needleleaf_evergreen',
                   'needleleaf_deciduous',
                   'c3_grass',
                   'c3_forb',
                   'c4',
                   'succulent',
                   'n_fixer')
custom_assign <- function(growth_form, ps_pathway, woodiness, phenology, leaf_type, n_fixation, climate_zone) {
    pft <- NA_character_
    # First try based on attributes
    # `isTRUE` is necessary to handle missing values
    if (isTRUE(growth_form == 'succulent' | ps_pathway == 'CAM')) {
        pft <- 'succulent'
    } else if (isTRUE(ps_pathway == 'C4')) {
        pft <- 'c4'
    } else if (isTRUE(n_fixation)) {
        pft <- 'n_fixer'
    } else if (isTRUE(woodiness == 'woody')) {
        if (isTRUE(leaf_type == 'broad')) {
            if (isTRUE(phenology == 'deciduous')) {
                if (isTRUE(climate_zone == 'tropical')) {
                    pft <- 'broadleaf_deciduous_tropical'
                } else if (isTRUE(climate_zone %in% c('temperate', 'boreal'))) {
                    pft <- 'broadleaf_deciduous_temperate'
                }
            } else if (isTRUE(phenology == 'evergreen')) {
                if (isTRUE(climate_zone == 'tropical')) {
                    pft <- 'broadleaf_evergreen_tropical'
                } else if (isTRUE(climate_zone %in% c('temperate', 'boreal'))) {
                    pft <- 'broadleaf_evergreen_temperate'
                }
            }
        } else if (isTRUE(leaf_type == 'needle')) {
            if (isTRUE(phenology == 'deciduous')) {
                pft <- 'needleleaf_deciduous'
            } else {
                pft <- 'needleleaf_evergreen'
            }
        }
    } else if (isTRUE(woodiness == 'nonwoody')) {
        if (isTRUE(growth_form == 'graminoid')) {
            pft <- 'c3_grass'
        } else if (isTRUE(growth_form == 'forb')) {
            pft <- 'c3_forb'
        }
    }
    pft_factor <- factor(pft, levels = custom_levels)
    return(pft_factor)
}
