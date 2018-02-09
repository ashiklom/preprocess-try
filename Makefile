.PHONY: all

all: diagnostics/summary.html

diagnostics/summary.html: R/summary.Rmd processed/traits/traits_analysis.rds .family
	Rscript -e 'rmarkdown::render("R/summary.Rmd")'
	mv R/summary.html diagnostics/

####################
# Directories
####################
processed/pfts/%.rds: processed/pfts
processed/traits/%: processed/traits
processed/species/%: processed/species

processed/pfts:
	mkdir -p $@

processed/traits:
	mkdir -p $@

processed/species:
	mkdir -p $@

processed/traits/Rd.rds: R/traits/Rd.R R/common.R
processed/traits/Jmax.rds: R/traits/Jmax.R R/common.R
processed/traits/Vcmax.rds: R/traits/Vcmax.R R/common.R
processed/traits/N_P_LL_SLA.rds: R/traits/N_P_LL_SLA.R R/common.R

processed/traits/trait_data.rds: R/traits/combine_traits.R \
	processed/traits/Rd.rds \
	processed/traits/Jmax.rds \
	processed/traits/Vcmax.rds \
	processed/traits/N_P_LL_SLA.rds

processed/species/theplantlist.rds: R/process_tpl.R
processed/species/tps_species.rds: R/merge_species.R processed/species/theplantlist.rds
.family: R/family.R processed/species/tps_species.rds

processed/pfts/%.rds: R/%.R pft_data/%.csv .family R/common.R
processed/pfts/growth_form.rds: R/growth_form.R
processed/pfts/ps_pathway.rds: R/ps_pathway.R
processed/pfts/leaf_type.rds: R/leaf_type.R
processed/pfts/phenology.rds: R/phenology.R
processed/pfts/n_fixation.rds: R/n_fixation.R
processed/traits/traits_with_climate.rds: R/climate.R pft_data/lat_lon_AMT.rds processed/traits/trait_data.rds
processed/pfts/climate_zone.rds: R/climate_zone.R processed/traits/traits_with_climate.rds
processed/pfts/lat_lon.rds: R/pfts/lat_lon.R R/common.R

processed/traits/traits_pfts.rds processed/species/all_pfts.rds: R/traits/make_pft.R R/pft_schemes.R \
    processed/pfts/growth_form.rds \
    processed/pfts/ps_pathway.rds \
    processed/pfts/leaf_type.rds \
    processed/pfts/phenology.rds \
    processed/pfts/n_fixation.rds \
    processed/pfts/climate_zone.rds
processed/traits/traits_analysis.rds: R/traits/finalize_traits.R processed/traits/traits_pfts.rds

####################
# General rules
####################
%::
	Rscript $<

