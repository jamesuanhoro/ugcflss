
# ugcflss

<!-- badges: start -->

[![R-CMD-check](https://github.com/jamesuanhoro/ugcflss/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jamesuanhoro/ugcflss/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/jamesuanhoro/ugcflss/branch/main/graph/badge.svg)](https://app.codecov.io/gh/jamesuanhoro/ugcflss?branch=main)
<!-- badges: end -->

ugcflss performs unadjusted group comparisons for data that are sum
scores of of several Likert items, and produces plots that help describe
patterns in the data. Models rely on Bayesian regularization and
hierarchical ordinal regression. Regularization and hierarchical
modeling help with stabilizing model parameters and inference about
quantities of interest. Ordinal regression helps with describing the
distribution of the data accurately.

## Installation

You can install ugcflss from [R-universe](https://r-universe.dev/) with:

``` r
install.packages('ugcflss', repos = 'https://ropensci.r-universe.dev')
```
