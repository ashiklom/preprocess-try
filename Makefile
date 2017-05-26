all: try_pfts.csv

phenology.R growth_form.R ps_pathway.R leaf_type.R: common.R

attributes/growth_form.csv: growth_form.R
	Rscript growth_form.R

attributes/ps_pathway.csv: ps_pathway.R
	Rscript ps_pathway.R

attributes/leaf_type.csv: leaf_type.R
	Rscript leaf_type.R

attributes/phenology.csv: phenology.R
	Rscript phenology.R

attributes/try_pfts.csv: make_pft.R attributes/growth_form.csv attributes/ps_pathway.csv attributes/leaf_type.csv attributes/phenology.csv
	Rscript make_pft.R
