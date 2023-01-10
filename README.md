
<!-- README.md is generated from README.Rmd. Please edit that file -->

# labNorm <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/labNorm)](https://CRAN.R-project.org/package=labNorm)
<!-- badges: end -->

`labNorm` provides functions for normalizing standard laboratory
measurements (e.g. hemoglobin, cholesterol levels) according to age and
sex. These normalizations are based on the algorithms described in the
research paper [“Personalized lab test models to quantify disease
potentials in healthy
individuals”](https://doi.org/10.1038/s41591-021-01468-6).

This package allows users to easily obtain normalized values for their
lab results and to project them on the population distribution. For more
information go to: <https://tanaylab.weizmann.ac.il/labs/>

## Installation

You can install the development version of `labNorm` from GitHub using
the `remotes` package:

``` r
retmotes::install_github("tanaylab/labNorm")
```

## Example

Normalize hemoglobin values for a group of subjects:

``` r
library(labNorm)

# Add a column for the normalized values
hemoglobin_data$quantile <- ln_normalize(
    hemoglobin_data$value,
    hemoglobin_data$age,
    hemoglobin_data$sex,
    "Hemoglobin"
)
#> → Using default quantiles. For higher resolution quantiles, run `ln_download_data()`. This message will only be shown once per session.

head(hemoglobin_data)
#>   age    sex value   quantile
#> 1  20   male  9.39 0.03838718
#> 2  20   male 14.03 0.18914947
#> 3  20   male 14.44 0.28589076
#> 4  20   male 15.80 0.75032395
#> 5  20 female 12.06 0.24222206
#> 6  20 female 12.89 0.55270091
```

Plot the quantiles vs values for age 50-60:

``` r
library(ggplot2)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

hemoglobin_data %>%
    filter(age >= 50 & age <= 60) %>%
    ggplot(aes(x = value, y = quantile, color = sex)) +
    geom_point() +
    theme_classic()
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

Plot the age/sex distribution of Hemoglobin:

``` r
ln_plot_dist("Hemoglobin")
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />
