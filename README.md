
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stratifiedmeasures

<!-- badges: start -->

<!-- badges: end -->

This is an R package that computes the crude odds/risk ratio (OR/RR) and
stratum-specific odds/risk ratios by a specified confounder using raw
exposure and outcome vectors.

## Installation

You can install the development version of stratifiedmeasures from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
library(devtools)
devtools::install_github("bernardshek01/stratifiedmeasures")
library(stratifiedmeasures)
```

## Example

``` r
library(stratifiedmeasures)

set.seed(123)
exposure   <- rbinom(100, 1, 0.4)
outcome    <- rbinom(100, 1, 0.3)
confounder <- sample(1:3, 100, replace = TRUE)

stratified_or(exposure, outcome, confounder)
stratified_rr(exposure, outcome, confounder)
```
