# extrapolators.R
# helper extrapolator functions for use in base year changes

#' extrapolate_constant
#'
#'  computes the mean of last n values and uses this constant value to fill in
#'  NA values corresponding to missing years at the end of the time series
#'
#' @param x Vector of values with NA's to be filled in via constant extrapolation
#' of the mean of last n non-NA values.
#' @param n Number of non-NA values to be averaged to provide the filler value.
#' Defaults to n=1: using the last recorded year's value to constantly fill in
#' the tail of vector missing values.
#' @param numMissing The number of NA values at the tail end of each
#' vector to be filled in. This will always be known for each data set in each
#' chunk.
#' @details Computes the mean of last n non-NA values of input vector x
#' and uses this constant value to fill in NA values in x.
#' @return Vector with all NA values replaced with the specified mean.
#' @importFrom assertthat assert_that is.scalar
#' @importFrom utils tail
#' @author ACS June 2019
extrapolate_constant <- function(x, n=1, numMissing){

  # Some assertion tests to make sure working on right data types
  assert_that(is.numeric(x))
  assert_that(is.scalar(n))
  assert_that(is.integer(numMissing))


  # The constant value to fill in all tail of vector NA's with.
  # = mean of the last n nonNA values in the
  meanval <- mean(last_n(x,n))


  # fill in only the tail end NA values with this constant.
  x[(length(x) - numMissing + 1):length(x)] <- meanval

  return(x)
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


