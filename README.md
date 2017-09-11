# mscelns
A number of my miscellaneous functions that I find useful. Currently included:

* **t_table**: Produces table of means, standard deviations, t-statistics, degrees of freedom, p-values, Cohen's ds, and confidence intervals for t-tests. It takes a list of variable names for the dependent variables.
* **mlm_ss**: Simple slope analyses for two-way interactions in two-level multilevel models.
* **ks_map**: County-level choropleth map generation for the state of Kansas.

## Installation
To install, you first need the `devtools` package. You can get this by installing it via CRAN:

`install.packages("devtools")`

Then, you can install this package by running:

`devtools::install_github("markhwhiteii/mscelns")`
