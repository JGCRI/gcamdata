# extrapolators.R
# helper extrapolator functions for use in base year changes

#' extrapolate_constant
#'
#'  computes the mean of last n original year values (with
#'  \code{mean(., na.rm = TRUE)}) and uses this constant value to fill in NA
#'  values corresponding to the extrapolation years at the end of the time series.
#'  NOTE that this extrapolator does not touch any of the original data. It ONLY
#'  fills in data corresponding to the extrapolation years. It is the user's
#'  responsibility to account for this behavior in preparing raw data to be
#'  extrapolated.
#'
#' @param x Vector of values with NA's corresponding to the extrapolation years
#' to be filled in via constant extrapolation of the mean of last n original
#' year values.
#' @param n Number of final original year values to be averaged to provide the
#' filler value. Averaging is done with \code{na.rm = TRUE}.
#' Defaults to n = 1: using the last recorded year's value to constantly fill in
#' the tail of vector missing values corresponding to extrapolation years.
#' @param numExtrapYrs The number of NA values at the tail end of each vector that
#' correspond to the extrapolation years and will be filled in. This will always
#' be known for each data set in each chunk.
#' @details Computes the mean of last n original year values of input vector x
#' and uses this constant value to fill in NA values in x that correspond to the
#' added extrapolation years.
#' @return Vector with all NA values replaced with the specified mean.
#' @importFrom assertthat assert_that is.scalar
#' @importFrom utils tail
#' @author ACS June 2019
extrapolate_constant <- function(x, n=1, numExtrapYrs){

  # Some assertion tests to make sure working on right data types
  assert_that(is.numeric(x))
  assert_that(is.scalar(n))
  assert_that(is.integer(numExtrapYrs))


  # The constant value to fill in all extrapolation year NA's with.
  # = mean(. , na.rm = TRUE) of the last n  values in the original
  # data.
  index_last_n_orig_yrs <- (length(x) - numExtrapYrs - n + 1):(length(x) - numExtrapYrs)
  meanval <- mean(x[index_last_n_orig_yrs], na.rm = TRUE)


  # fill in only the tail end, extrapolation years
  # NA values with this constant.
  index_extrap_yrs <- (length(x) - numExtrapYrs + 1):length(x)
  x[index_extrap_yrs] <- meanval

  return(x)
}



#' last_n_nonNA
#'
#'  finds the last n non-NA values in an input vector.
#'  A convenience functions for users who wish to customize
#'  their extrapolations beyond the default or who wish to
#'  identify NA values in their original (unextrapolated)
#'  data.
#'
#' @param x Vector with some NA values
#' @param n The number of non-NA values sought.
#' @details finds the last n non-NA values in an input vector.
#' @return A vector with the last n non-NA values from input
#' vector x.
#' @importFrom assertthat assert_that is.scalar
#' @author ACS June 2019
last_n_nonNA <- function(x, n){

  assert_that(is.scalar(n))

  if(n > length(x[!is.na(x)])){
    stop('asking for more nonNA years than you have.')
  }


  return(tail(x[!is.na(x)], n))
}


