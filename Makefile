all: summary.html

summary.html: R/summary.Rmd processed/traits/traits_analysis.rds .family
	Rscript -e 'rmarkdown::render("R/summary.Rmd")'

# Directories
processed/pfts/%.rds: processed/pfts

processed/pfts:
	mkdir -p $@

processed/traits/%: processed/traits

processed/traits:
	mkdir -p $@

processed/species/%: processed/species

processed/species:
	mkdir -p $@

R/phenology.R R/growth_form.R R/ps_pathway.R R/leaf_type.R: R/common.R

# Species attributes
processed/pfts/%.rds: R/%.R
	Rscript $<

processed/pfts/growth_form.rds: R/growth_form.R pft_data/growth_form.csv .family
processed/pfts/ps_pathway.rds: R/ps_pathway.R pft_data/ps_pathway.csv .family
processed/pfts/leaf_type.rds: R/leaf_type.R pft_data/leaf_type.csv .family
processed/pfts/phenology.rds: R/phenology.R pft_data/phenology.csv .family
processed/pfts/n_fixation.rds: R/n_fixation.R pft_data/n_fixation.csv .family
processed/pfts/climate_zone.rds: R/climate_zone.R processed/traits/traits_with_climate.rds

processed/pfts/lat_lon.rds: R/pfts/lat_lon.R R/common.R
	Rscript $<

# Trait data
processed/traits/%.rds: R/traits/%.R
	Rscript $<

processed/traits/Rd.rds: R/traits/Rd.R R/common.R
processed/traits/Jmax.rds: R/traits/Jmax.R R/common.R
processed/traits/Vcmax.rds: R/traits/Vcmax.R R/common.R
processed/traits/N_P_LL_SLA.rds: R/traits/N_P_LL_SLA.R R/common.R

processed/traits/traits_analysis.rds: R/traits/finalize_traits.R processed/traits/traits_pfts.rds
	Rscript $<

processed/traits/traits_pfts.rds processed/species/all_pfts.rds: R/traits/make_pft.R R/pft_schemes.R \
    processed/pfts/growth_form.rds \
    processed/pfts/ps_pathway.rds \
    processed/pfts/leaf_type.rds \
    processed/pfts/phenology.rds \
    processed/pfts/n_fixation.rds \
    processed/pfts/climate_zone.rds
	Rscript $<

processed/traits/traits_with_climate.rds: R/climate.R pft_data/lat_lon_AMT.rds processed/traits/trait_data.rds
	Rscript R/climate.R

processed/traits/trait_data.rds: R/trait_data.R
	Rscript R/trait_data.R

processed/species/tps_species.rds: R/merge_species.R processed/species/theplantlist.rds
	Rscript R/merge_species.R

processed/species/theplantlist.rds: R/process_tpl.R
	Rscript R/process_tpl.R

.family: R/family.R processed/species/tps_species.rds
	Rscript R/family.R
