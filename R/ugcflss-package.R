#' The 'ugcflss' package.
#'
#' @description Performs unadjusted group comparisons for data that are
#' sum scores of several Likert items, and produces plots that help describe
#' patterns in the data. Models rely on Bayesian regularization and
#' hierarchical ordinal regression. Regularization and hierarchical modeling
#' help with stabilizing model parameters and inference about quantities of
#' interest. Ordinal regression helps with describing the distribution of the
#' data accurately.
#'
#' For a demo, check out the vignette, \code{vignette("ugcflss_tutorial")}
#'
#' @name ugcflss-package
#' @aliases ugcflss
#' @useDynLib ugcflss, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @importFrom rstan sampling
#' @importFrom rstantools rstan_config
#' @importFrom RcppParallel RcppParallelLibs
#'
#' @references
#' Stan Development Team (NA). RStan: the R interface to Stan. R package version 2.32.6. https://mc-stan.org
#'
NULL
