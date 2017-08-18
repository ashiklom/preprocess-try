all: summary.html

summary.html: summary.Rmd traits/traits_analysis.rds .family
	Rscript -e 'rmarkdown::render("summary.Rmd")'

# Directories
attributes/%.rds: attributes

attributes:
	mkdir -p $@

traits/%: traits

traits:
	mkdir -p $@

pfts_species/%: pfts_species

pfts_species:
	mkdir -p $@

climate/%: climate

climate:
	mkdir -p $@

phenology.R growth_form.R ps_pathway.R leaf_type.R: common.R

# Species attributes
attributes/%.rds: %.R
	Rscript $<

attributes/growth_form.rds: growth_form.R attribute_maps/growth_form.csv .family
attributes/ps_pathway.rds: ps_pathway.R attribute_maps/ps_pathway.csv .family
attributes/leaf_type.rds: leaf_type.R attribute_maps/leaf_type.csv .family
attributes/phenology.rds: phenology.R attribute_maps/phenology.csv .family
attributes/n_fixation.rds: n_fixation.R attribute_maps/n_fixation.csv .family
attributes/climate_zone.rds: climate_zone.R traits/traits_with_climate.rds

traits/traits_analysis.rds traits/traits_pfts.rds pfts_species/all_pfts.rds: make_pft.R pft_schemes.R \
    attributes/growth_form.rds \
    attributes/ps_pathway.rds \
    attributes/leaf_type.rds \
    attributes/phenology.rds \
    attributes/n_fixation.rds \
    attributes/climate_zone.rds
	Rscript make_pft.R

traits/traits_with_climate.rds: climate.R climate/try_temperature_values.rds traits/trait_data.rds
	Rscript climate.R

traits/trait_data.rds: trait_data.R
	Rscript trait_data.R

pfts_species/tps_species.rds: merge_species.R pfts_species/theplantlist.rds
	Rscript merge_species.R

pfts_species/theplantlist.rds: process_tpl.R
	Rscript process_tpl.R

.family: family.R pfts_species/tps_species.rds
	Rscript family.R
