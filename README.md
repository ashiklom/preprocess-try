Workflow for processing TRY data

**Author:** Alexey Shiklomanov

# Organization

- `README.md` -- Description of pre-processing workflow
- `Makefile` -- Annotated Makefile
- `R` -- Scripts for processing data
    - `species/` -- Scripts and data for working with species
        - `process_tpl.R` -- Download and format ThePlantList data
        - `merge_species.R` -- Merge ThePlantList species list with TRY database
        - `family.R` -- Use `taxize` to retrieve species phylogeny
    - `pfts/` -- Scripts and data for assigning attributes to PFTs
    - `traits/` -- Scripts for extracting traits from TRY
- `pft_data/*.csv` -- CSV files containing maps between TRY data and my attributes

# Prerequisites

The file `try.sqlite` must exist in the root directory.
Because of the TRY data sharing policy, I cannot provide a public link to this file.

In addition, the following R packages are required:

- `tidyverse`
- `here`
- `data.table`
- `taxize`
- `knitr`
- `rmarkdown`
