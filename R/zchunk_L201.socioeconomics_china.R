#' module_gcam.china_L201.socioeconomics
#'
#' Interest rate, population, and GDP for GCAM-China.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L201.InterestRate_CHINA}, \code{L201.Pop_GCAMCHINA}, \code{L201.BaseGDP_GCAMCHINA}, \code{L201.LaborForceFillout_CHINA}, \code{L201.LaborProductivity_GCAMCHINA}. The corresponding file in the
#' original data system was \code{L201.socioeconomics_CHINA.R} (gcam-China level2).
#' @details Interest rate, population, and GDP for GCAM-China.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YangLiu Aug 2018
module_gcam.china_L201.socioeconomics <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/province_names_mappings",
             "L100.Pop_thous_province",
             "L100.GDP_mil90usd_province",
             "L102.pcgdp_thous90USD_GCAM3_ctry_Y",
             "L100.pcGDP_thous90usd_province"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L201.InterestRate_CHINA",
             "L201.Pop_GCAMCHINA",
             "L201.BaseGDP_GCAMCHINA",
             "L201.LaborForceFillout_CHINA",
             "L201.LaborProductivity_GCAMCHINA"))
  } else if(command == driver.MAKE) {

    # silence package checks
    year <- value <- province <- totalPop <- baseGDP <- iso <- growth <- timestep <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    province_names_mappings <- get_data(all_data, "gcam-china/province_names_mappings")
    L100.Pop_thous_province <- get_data(all_data, "L100.Pop_thous_province")
    L100.GDP_mil90usd_province <- get_data(all_data, "L100.GDP_mil90usd_province")
    L102.pcgdp_thous90USD_GCAM3_ctry_Y <- get_data(all_data, "L102.pcgdp_thous90USD_GCAM3_ctry_Y")
    L100.pcGDP_thous90usd_province <- get_data(all_data, "L100.pcGDP_thous90usd_province")

    # ===================================================
    # L201.InterestRate: Interest rates by region
    L201.InterestRate <- tibble(region = provinces_subregions$province, interest.rate = socioeconomics.DEFAULT_INTEREST_RATE)

    # L201.Pop_GCAMChina: Population by region from the GCAM 3.0 core scenario
    L201.Pop_GCAMCHINA <- L100.Pop_thous_province %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(totalPop = value,
             region = province) %>%
      mutate(totalPop = round(totalPop, socioeconomics.POP_DIGITS))

    # L201.BaseGDP_GCAMChina: Base GDP for GCAM-China scenario
    L201.BaseGDP_GCAMCHINA <- L100.GDP_mil90usd_province %>%
      filter(year == min(MODEL_YEARS)) %>%
      rename(baseGDP = value,
             region = province) %>%
      mutate(baseGDP = round(baseGDP, socioeconomics.GDP_DIGITS)) %>%
      select(-year)

    # L201.LaborForceFillout: Labor force participation and productivity for all scenarios
    # NOTE: No model of labor force used; labor force participation set to a constant
    L201.LaborForceFillout_CHINA <- tibble(region = provinces_subregions$province,
                                     year.fillout = min(MODEL_YEARS),
                                     laborforce = socioeconomics.DEFAULT_LABORFORCE)

    # LABOR PRODUCTIVITY GROWTH RATE CALCULATION
    # Labor productivity growth is calculated from the change in per-capita GDP ratio in each time period
    # Calculate the growth rate in per-capita GDP
    L201.LaborProductivity_GCAMCHINA <- L102.pcgdp_thous90USD_GCAM3_ctry_Y %>%
      filter(tolower(iso) == "China",
             year %in% MODEL_YEARS) %>%
      # In order to calculate growth rate we need to know how much GDP grew and number of years between periods
      mutate(growth = value / lag(value),
             timestep = year - lag(year),
             laborproductivity = round(growth ^ (1 / timestep) - 1, socioeconomics.LABOR_PRODUCTIVITY_DIGITS)) %>%
      # Remove the first model year, since it has no previous period to calculate growth rate
      filter(year != min(MODEL_YEARS)) %>%
      # Renaming to region because region column needed for write_to_all_provinces
      rename(region = iso) %>%
      # NOTE: applying the China average to all provinces equally
      write_to_all_provinces(LEVEL2_DATA_NAMES[["LaborProductivity"]])

    # ===================================================

    # Produce outputs
    L201.InterestRate %>%
      add_title("Interest rates by province") %>%
      add_units("Unitless") %>%
      add_comments("Constant assumed for all provinces") %>%
      add_legacy_name("L201.InterestRate_GCAMChina") %>%
      add_precursors("gcam-China/provinces_subregions") ->
      L201.InterestRate_CHINA

    L201.Pop_GCAMCHINA %>%
      add_title("Population by province") %>%
      add_units("thoChinand persons") %>%
      add_comments("Data from L100.Pop_thous_province") %>%
      add_legacy_name("L201.Pop_GCAMChina") %>%
      add_precursors("L100.Pop_thous_province") ->
      L201.Pop_GCAMCHINA

    L201.BaseGDP_GCAMCHINA %>%
      add_title("Base year GDP by province") %>%
      add_units("million 1990 USD") %>%
      add_comments("Data from L100.GDP_mil90usd_province") %>%
      add_legacy_name("L201.BaseGDP_GCAMChina") %>%
      add_precursors("L100.GDP_mil90usd_province") ->
      L201.BaseGDP_GCAMCHINA

    L201.LaborForceFillout %>%
      add_title("Labor force participation and productivity for all scenarios") %>%
      add_units("Unitless") %>%
      add_comments("Constant value assumed") %>%
      add_legacy_name("L201.LaborForceFillout") %>%
      add_precursors("gcam-China/provinces_subregions") ->
      L201.LaborForceFillout_CHINA

    L201.LaborProductivity_GCAMChina %>%
      add_title("Labor force productivity growth rate for GCAM-China") %>%
      add_units("Unitless (annual rate of growth)") %>%
      add_comments("Values from L102.pcgdp_thous90USD_GCAM3_ctry_Y used to calculate annual growth") %>%
      add_comments("China value written to all provinces") %>%
      add_legacy_name("L201.LaborProductivity_GCAMChina") %>%
      add_precursors("L102.pcgdp_thous90USD_GCAM3_ctry_Y") ->
      L201.LaborProductivity_GCAMCHINA


    return_data(L201.InterestRate_CHINA, L201.Pop_GCAMCHINA, L201.BaseGDP_GCAMCHINA, L201.LaborForceFillout_CHINA, L201.LaborProductivity_GCAMCHINA)
  } else {
    stop("Unknown command")
  }
}
