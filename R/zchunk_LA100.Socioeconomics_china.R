#' module_gcam.china_LA100.Socioeconomics
#'
#' This chunk calculates the historical GDP, per-capita GDP and population by province.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L100.pcGDP_thous90usd_province}, \code{L100.GDP_mil90usd_province}, \code{L100.Pop_thous_province}. The corresponding file in the
#' original data system was \code{LA100.Socioeconomics.R} (gcam-china level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom tibble tibble
#' @import dplyr
#' @importFrom tidyr gather spread
#' @author RC Jul 2018
#' @export
module_gcam.china_LA100.Socioeconomics <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/province_names_mappings",
             FILE = "gcam-china/population_thous_province",
             FILE = "gcam-china/gdp_mil10usd_province"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L100.pcGDP_thous90usd_province",
             "L100.GDP_mil90usd_province",
             "L100.Pop_thous_province"))
  } else if(command == driver.MAKE) {

    year <- GDP <- province.name <- province <- pop <- pcGDP <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    province_names_mappings   <- get_data(all_data, "gcam-china/province_names_mappings")
    population_thous_province <- get_data(all_data, "gcam-china/population_thous_province")
    gdp_mil10usd_province     <- get_data(all_data, "gcam-china/gdp_mil10usd_province")

    # Get a list of the years for socioeconomic variables
    socio_hist_years <- as.integer(grep("[0-9]{4}", names(gdp_mil10usd_province), value = TRUE))

    # Fill missing values in GDP
    gdp_mil10usd_province %>%
      # Convert to long format
      gather(year, GDP, matches("[0-9]{4}")) %>%
      mutate(year = as.integer(year)) %>%
      complete(nesting(province.name), year = socio_hist_years) %>%
      arrange(province.name, year) %>%
      group_by(province.name) %>%
      # Note that missing values are interpolated if bounded and filled with the next closest value otherwise.
      mutate(GDP = approx_fun(year, GDP),
             # Convert 2010 USD to 1990 USD
             GDP = GDP * gdp_deflator(1990, base_year = 2010)) %>%
      ungroup %>%
      map_province_name(province_names_mappings, "province", TRUE) ->
      L100.GDP_mil90usd_province


    # Fill missing values in population
    # Note that missing values are interpolated if bounded and filled  with the next closest value otherwise.
    population_thous_province %>%
      gather(year, pop, matches("[0-9]{4}")) %>%
      mutate(year = as.integer(year)) %>%
      complete(nesting(province.name), year = socio_hist_years) %>%
      arrange(province.name, year) %>%
      group_by(province.name) %>%
      mutate(pop = approx_fun(year, pop)) %>%
      ungroup %>%
      map_province_name(province_names_mappings, "province", TRUE) ->
      L100.Pop_thous_province

    # Calculate GDP per capita
    L100.GDP_mil90usd_province %>%
      left_join_error_no_match(L100.Pop_thous_province, by = c("province", "year")) %>%
      mutate(pcGDP = GDP / pop) %>%
      replace_na(list(pcGDP = 0)) %>%
      select(province, year, pcGDP) ->
      L100.pcGDP_thous90usd_province

    # Produce outputs
    L100.pcGDP_thous90usd_province %>%
      add_title("Per-capita GDP by province") %>%
      add_units("thousand 1990 USD per capita") %>%
      add_comments("historical and future years") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L100.pcGDP_thous90usd_province") %>%
      add_precursors("gcam-china/province_names_mappings",
                     "gcam-china/population_thous_province",
                     "gcam-china/gdp_mil10usd_province") ->
      L100.pcGDP_thous90usd_province

    L100.GDP_mil90usd_province %>%
      add_title("GDP by province") %>%
      add_units("million 1990 USD") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L100.GDP_mil90usd_province") %>%
      add_precursors("gcam-china/province_names_mappings",
                     "gcam-china/gdp_mil10usd_province") ->
      L100.GDP_mil90usd_province

    L100.Pop_thous_province %>%
      add_title("Population by province") %>%
      add_units("thousand persons") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L100.Pop_thous_province") %>%
      add_precursors("gcam-china/province_names_mappings",
                     "gcam-china/population_thous_province") ->
      L100.Pop_thous_province

    return_data(L100.pcGDP_thous90usd_province, L100.GDP_mil90usd_province, L100.Pop_thous_province)
  } else {
    stop("Unknown command")
  }
}
