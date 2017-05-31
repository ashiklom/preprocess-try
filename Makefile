all: summary.html

phenology.R growth_form.R ps_pathway.R leaf_type.R: common.R

attributes/%.csv: %.R
	Rscript $<

attributes/growth_form.csv: growth_form.R .family

attributes/ps_pathway.csv: ps_pathway.R .family

attributes/leaf_type.csv: leaf_type.R .family

attributes/phenology.csv: phenology.R .family

try_pfts.csv: make_pft.R \
    attributes/growth_form.csv \
    attributes/ps_pathway.csv \
    attributes/leaf_type.csv \
    attributes/phenology.csv
	Rscript make_pft.R

trait_data.rds: trait_data.R try_pfts.csv
	Rscript trait_data.R

summary.html: summary.Rmd trait_data.rds .family
	Rscript -e 'rmarkdown::render("summary.Rmd")'

.family: family.R theplantlist.rds
	Rscript family.R

theplantlist.rds: process_tpl.R
	Rscript process_tpl.R
