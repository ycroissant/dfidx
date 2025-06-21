#' dfidx package: linear models for panel data
#'
#' ## Overview
#'
#' `dfidx` is a small `R` package which is intended to deal
#' efficiently with data frames when each observation is identified by
#' a combination of values of two variables (indexes). `dfidx` exports
#' the function `dfidx` which takes as main argument a data frame and
#' returns a `dfidx` object and the `idx`/`idx_name` functions which
#' respectively return one or all of the indexes and their
#' names. Methods are provided for `R`'s extractors.
#' 
#' ## Installation
#' 
#' `dfidx` is on `CRAN`. Use: `install.packages("dfidx")`.
#'
#' To install the developping version, use:
#' `remotes::install_github("ycroissant/dfidx")`.
#' 
#' @name dfidx-package
#' @docType package
#' @keywords package
