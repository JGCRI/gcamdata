#' module_gcam.china_LA114.Wind
#'
#' Compute capacity factors for wind by China province.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L114.CapacityFactor_wind_province}. The corresponding file in the
#' original data system was \code{LA114.Wind.R} (gcam-usa level1).
#' @details Computes capacity factors for wind by US state.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author ST September 2017
#' @export
module_gcam.china_LA114.Wind <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( FILE = "gcam-china/wind_potential_province",
              FILE = "energy/A23.globaltech_capital",
              FILE = "energy/A23.globaltech_OMfixed",
              FILE = "gcam-china/province_names_mappings",
              FILE = "energy/A23.globaltech_OMvar"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L114.CapacityFactor_wind_province"))
  } else if(command == driver.MAKE) {

    technology <- year <- state <- sector <- capacity.factor <- fuel <- value <- base_cost <-
      region <- NULL  # silence package check.

    all_data <- list(...)[[1]]

    # -----------------------------------------------------------------------------
    # 1.Load required inputs

    province_names_mappings <- get_data(all_data, "gcam-china/province_names_mappings")
    wind_potential_province <- get_data(all_data, "gcam-china/wind_potential_province")
    A23.globaltech_capital  <- get_data(all_data, "energy/A23.globaltech_capital")
    A23.globaltech_OMfixed  <- get_data(all_data, "energy/A23.globaltech_OMfixed")
    A23.globaltech_OMvar    <- get_data(all_data, "energy/A23.globaltech_OMvar")

    # -----------------------------------------------------------------------------
    # 2.perform computations

    # This function filters A23 tables for wind, then gathers and interpolates...
    # ... to get a single value for wind base cost year. Note that the interpolation is...
    # ... redundant whilst gcamusa.WIND_BASE_COST_YEAR = 2005, ...
    # ... since 2005 is an existing data point in all A23 tables.
    filter_gather_interp_get_cost <- function(x) {
      . <- NULL  # silence package check notes
      x %>% filter(technology == "wind") %>%
        gather_years %>%
        select(year, value) %>%
        complete(year = c(year, gcamchina.WIND_BASE_COST_YEAR)) %>%
        mutate(value = approx_fun(year, value, rule = 2)) %>%
        filter(year == gcamchina.WIND_BASE_COST_YEAR) %>%
        pull(value)
    }

    #Extract the costs. These are in 1975$
    A23.globaltech_capital %>% filter_gather_interp_get_cost -> CapCost
    A23.globaltech_OMfixed %>% filter_gather_interp_get_cost -> OMFixedCost
    A23.globaltech_OMvar %>% filter_gather_interp_get_cost -> OMVarCost


    # Get fixed charge rate of capital for wind
    filter(A23.globaltech_capital, technology == "wind")$fixed.charge.rate -> FixedChargeRate

    #Get the base cost by province in the correct units (from 2007$/kWh to 1975$/GJ)
    #Calculate the capacity factor for the base wind turbine in each province
    wind_potential_province %>%
      mutate(sector = "electricity generation",
             fuel = "wind",
             base.cost = base.cost * gdp_deflator(1975, 2007) / CONV_KWH_GJ,
             capacity.factor = (CapCost * FixedChargeRate + OMFixedCost) /
               (CONV_KWH_GJ * CONV_YEAR_HOURS) / (base.cost - (OMVarCost / (1000 * CONV_KWH_GJ)))) %>%
      select(province.name, sector, fuel, capacity.factor) ->k
    k %>%
      map_province_name(province_names_mappings, "province", TRUE) ->
      L114.CapacityFactor_wind_province

    # -----------------------------------------------------------------------------
    # 3.Produce outputs
    L114.CapacityFactor_wind_province %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L114.CapacityFactor_wind_province") %>%
      add_precursors("gcam-china/wind_potential_province",
                     "gcam-china/province_names_mappings",
                     "energy/A23.globaltech_capital",
                     "energy/A23.globaltech_OMfixed",
                     "energy/A23.globaltech_OMvar") ->
      L114.CapacityFactor_wind_province

    return_data(L114.CapacityFactor_wind_province)
  } else {
    stop("Unknown command")
  }
}
