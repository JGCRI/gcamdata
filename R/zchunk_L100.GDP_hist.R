# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socioeconomics_L100.GDP_hist
#'
#' Prepare GDP database for later use: filter missing values and convert units to 1990 USD.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L100.gdp_mil90usd_ctry_Yh}. The corresponding file in the
#' original data system was \code{L100.GDP_hist.R} (socioeconomics level1).
#' @details At present the GDP database used requires no downscaling and all
#' major countries are included, so really no processing steps are needed.
#' All that happens in this file right now is filtering out \code{NA} values
#' and converting the units to GCAM's GDP unit (million 1990 USD).
#' @importFrom assertthat assert_that
#' @importFrom tibble tibble
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread complete
#' @author BBL February 2017
module_socioeconomics_L100.GDP_hist <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "socioeconomics/USDA_GDP_MER"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L100.gdp_mil90usd_ctry_Yh"))
  } else if(command == driver.MAKE) {

    Country <- year <- value <- iso <- NULL # silence package checks.

    all_data <- list(...)[[1]]

    # Load required inputs
    usda_gdp_mer <- get_data(all_data, "socioeconomics/USDA_GDP_MER")
    assert_that(tibble::is.tibble(usda_gdp_mer))

    # Convert to long form, convert units
    usda_gdp_mer %>%
      select(-Country) %>%
      gather_years %>%
      filter(!is.na(value), !is.na(iso)) %>%
      mutate(value = value * CONV_BIL_MIL * gdp_deflator(1990, base_year = 2010),
             year = as.integer(year)) ->
      long_iso_year_gdp


    # Perform BYU
    # Skeleton - constant extrapolation
    # BYUcompliant
    #
    # BYU NOTE - must make sure the units are all 1990 USD (or at least the
    # same year basis) so averaging behaves. Fine here, but worth noting
    # across other chunk where money comes up, especially since we drop
    # unit information pretty early on in most chunks.
    # BYU NOTE - need to think about labeling that the output has BYU
    # update done, and a note about the method?
    if(max(long_iso_year_gdp$year) < BYU_YEAR){

      missingyears <- (max(long_iso_year_gdp$year) +  1):BYU_YEAR

      # Constant extrapolation operating only on
      # numeric vector. Can operate on any numeric vector, regardless of grouping
      # or column name.
      # What gets output in this chunk
      long_iso_year_gdp %>%
        complete(year = c(year, missingyears), iso) %>%
        group_by(iso) %>%
        mutate(value = extrapolate_constant(value, n = 1,
                                            numMissing = length(missingyears))) %>%
        ungroup ->
        long_iso_year_gdp
    }


    # filter to historical years, convert units
    long_iso_year_gdp %>%
      add_title("Historical GDP downscaled to country (iso)") %>%
      add_comments("Units converted to constant 1990 USD") %>%
      add_precursors("socioeconomics/USDA_GDP_MER") %>%
      add_units("Million 1990 USD") %>%
      add_legacy_name("L100.gdp_mil90usd_ctry_Yh") ->
      L100.gdp_mil90usd_ctry_Yh

    return_data(L100.gdp_mil90usd_ctry_Yh)
  } else {
    stop("Unknown command")
  }
}
