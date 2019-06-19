# extrapolators.R
# helper extrapolator functions for use in base year changes

#' extrapolate_constant
#'
#'  computes the mean of last n values and uses this constant value to fill in
#'  missing years at the end of the time series
#'
#' @param x Vector of values with NA's to be filled in via constant extrapolation
#' of the mean of last n non-NA values.
#' @param n Number of non-NA values to be averaged to provide the filler value.
#' @details Computes the mean of last n non-NA values of input vector x
#' and uses this constant value to fill in NA values in x.
#' @return Vector with all NA values replaced with the specified mean.
#' @importFrom assertthat assert_that is.scalar
#' @author ACS June 2019
extrapolate_constant <- function(x, n=1){

  assert_that(is.numeric(x))
  assert_that(is.scalar(n))

  meanval <- mean(last_n(x,n))

  x[is.na(x)] <- meanval

  return(x)
}


#' extrapolate_constant_optionB
#'
#'  computes the mean of last n values and uses this constant value to fill in
#'  missing years at the end of the time series
#'
#' @param x Tibble with column name 'value' that contains NA values to be filled in
#' via constant extrapolation of the mean of last n non-NA values.
#' @param n Number of non-NA values to be averaged to provide the filler value.
#' @details Computes the mean of last n non-NA values of column 'value' in an input tibble
#' (may be grouped)  and uses this constant value to fill in NA values in column 'value'.
#' @return Tibble with column name 'value' in which all NA values have been filled
#' in with the specified mean value.
#' @importFrom assertthat assert_that is.scalar
#' @author ACS June 2019
extrapolate_constant_optionB <- function(x, n=1){

  assert_that(is.tibble(x))
  assert_that(is.scalar(n))


  # can't use the faster replace_na function because
  # if x is grouped, e.g. on iso, each group will have
  # its own, specific mean value to replace NAs with.
  x %>%
    mutate(meanval = mean(last_n(value, n)),
           value = if_else(is.na(value),
                           meanval,
                           value)) %>%
    select(-meanval) %>%
    distinct
}


#' last_n
#'
#'  finds the last n non-NA values in an input vector.
#'
#' @param x Vector with some NA values
#' @param n The number of non-NA values sought.
#' @details finds the last n non-NA values in an input vector.
#' @return A vector with the last n non-NA values from input
#' vector x.
#' @importFrom assertthat assert_that is.scalar
#' @author ACS June 2019
last_n <- function(x, n){

  assert_that(is.scalar(n))

  if(n > length(x[!is.na(x)])){
    stop('asking for more nonNA years than you have.')
  }


  return(tail(x[!is.na(x)], n))
}


