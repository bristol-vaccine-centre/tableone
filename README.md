
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tableone: Descriptive Tables for Observational or Interventional Studies <a href='https://bristol-vaccine-centre.github.io/tableone/index.html'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/bristol-vaccine-centre/tableone/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bristol-vaccine-centre/tableone/actions/workflows/R-CMD-check.yaml)
[![DOI](https://zenodo.org/badge/551386697.svg)](https://zenodo.org/badge/latestdoi/551386697)
[![tableone status
badge](https://bristol-vaccine-centre.r-universe.dev/badges/tableone)](https://bristol-vaccine-centre.r-universe.dev)
<!-- badges: end -->

The goal of `tableone` is to make it easy to generate comparison tables
for journal publication. It converts a line list of experimental or
observational data into a summary table which can be grouped by an
intervention. Reporting summaries of this kind of data has to be aware
of missing items, and provide summary statistics and statistical
comparisons that are appropriate for the data. This is amenable to some
automated decision making but frequently such automation must be
overridden. `tableone` provides an automated one command statistical
summary table the output of which is highly configurable. The resulting
tables are in `huxtable` format ready for export into a wide range out
file types.

## Installation

There is a different `tableone` package on CRAN. This package is hosted
in the [Bristol Vaccine Centre
r-universe](https://https://bristol-vaccine-centre.r-universe.dev/).
Installation from there is as follows:

``` r
options(repos = c(
  "bristol-vaccine-centre" = 'https://https://bristol-vaccine-centre.r-universe.dev/',
  CRAN = 'https://cloud.r-project.org'))

# Download and install tableone in R
install.packages("tableone")
```

You can install the development version of `tableone` from
[GitHub](https://github.com/bristol-vaccine-centre/tableone) with:

``` r
# install.packages("devtools")
devtools::install_github("bristol-vaccine-centre/tableone")
```

## Example

`tableone` is there to make descriptive statistics consistent and easy.

``` r
library(tidyverse)
#> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
#> ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
#> ✔ tibble  3.1.8      ✔ dplyr   1.0.10
#> ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
#> ✔ readr   2.1.2      ✔ forcats 0.5.2 
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
library(tableone)
#> 
#> Attaching package: 'tableone'
#> 
#> The following object is masked from 'package:ggplot2':
#> 
#>     diamonds
```

Summarising the dataset in a a nicely formatted summary table (although
this is not seen at its best in a github readme) is as simple as the
following code. For the proper formatted output head to the [main
documentation
website](https://bristol-vaccine-centre.github.io/tableone/).

``` r
# hide messages 
old = options(tableone.quiet = TRUE)


# generate table 
iris %>% 
  describe_population(everything())
#> Warning in to_md.huxtable(structure(list(variable = c("variable",
#> "Sepal.Length", : Markdown cannot handle cells with colspan/rowspan > 1
```

|         variable         | characteristic          | value                 | count (N=150) |
|:------------------------:|:------------------------|:----------------------|:--------------|
|       Sepal.Length       | Mean ± SD               | 5.84 ± 0.828          | 150           |
|       Sepal.Width        | Mean ± SD               | 3.06 ± 0.436          | 150           |
|       Petal.Length       | Median \[IQR\]          | 4.35 \[1.6—5.1\]      | 150           |
|       Petal.Width        | Median \[IQR\]          | 1.3 \[0.3—1.8\]       | 150           |
|         Species          | setosa % \[95% CI\]     | 33.3% \[26.3%—41.2%\] | 50/150        |
|                          | versicolor % \[95% CI\] | 33.3% \[26.3%—41.2%\] | 50/150        |
|                          | virginica % \[95% CI\]  | 33.3% \[26.3%—41.2%\] | 50/150        |
| Normality of distri test | butions determined      | using Anderson-Darl   | ing normality |

``` r

# reset options
options(old)
```

As a `huxtable` output the table can be saved as a wide range of formats
from spreadsheets or documents to latex and html (even as a github
README.md with limited success). This default output of `tableone` can
be very substantially customised to fit into a specific journal’s
requirements.
