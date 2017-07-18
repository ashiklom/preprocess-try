all: summary.html

phenology.R growth_form.R ps_pathway.R leaf_type.R: common.R

attributes/%.rds: %.R
	Rscript $<

attributes/growth_form.rds: growth_form.R attribute_maps/growth_form.csv .family

attributes/ps_pathway.rds: ps_pathway.R attribute_maps/ps_pathway.csv .family

attributes/leaf_type.rds: leaf_type.R attribute_maps/leaf_type.csv .family

attributes/phenology.rds: phenology.R attribute_maps/phenology.csv .family

attributes/n_fixation.rds: n_fixation.R attribute_maps/n_fixation.csv .family

attributes/climate_zone.rds: climate_zone.R traits_with_climate.rds

traits_pfts.rds all_pfts.rds: make_pft.R \
    attributes/growth_form.rds \
    attributes/ps_pathway.rds \
    attributes/leaf_type.rds \
    attributes/phenology.rds \
    attributes/n_fixation.rds \
    attributes/climate_zone.rds
	Rscript make_pft.R

traits_with_climate.rds: climate.R try_temperature_values.rds trait_data.rds
	Rscript climate.R

tps_species.rds: merge_species.R theplantlist.rds
	Rscript merge_species.R

.family: family.R tps_species.rds
	Rscript family.R

trait_data.rds: trait_data.R
	Rscript trait_data.R

summary.html: summary.Rmd traits_pfts.rds .family
	Rscript -e 'rmarkdown::render("summary.Rmd")'

theplantlist.rds: process_tpl.R
	Rscript process_tpl.R
