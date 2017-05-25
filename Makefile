all: try_pfts.csv

phenology.R growth_form.R ps_pathway.R leaf_type.R: common.R

growth_form.csv: growth_form.R
	Rscript growth_form.R

ps_pathway.csv: ps_pathway.R
	Rscript ps_pathway.R

leaf_type.csv: leaf_type.R
	Rscript leaf_type.R

phenology.csv: phenology.R
	Rscript phenology.R

try_pfts.csv: make_pft.R growth_form.csv ps_pathway.csv leaf_type.csv phenology.csv
	Rscript make_pft.R
