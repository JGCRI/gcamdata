#' module_gcamusa_LB174.nonghg_bld
#'
#' Calculate non-ghg emission totals and non-ghg emission shares of total emissions.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L174.nonghg_tg_state_bld_F_Yb}. The corresponding file in the
#' original data system was \code{LB174.nonghg_bld_USA.R} (gcam-usa level1).
#' @details This chunk isolates buildings sector non-ghg input emissions by U.S. state / sector / fuel / pollutant / year from NEI 2011.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YO July 2018
module_gcamusa_LB174.nonghg_bld <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/emissions/NEI_pollutant_mapping",
             FILE = "gcam-usa/emissions/CEDS_GCAM_fuel",
             FILE = "gcam-usa/emissions/NEI_2011_GCAM_sectors"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L174.nonghg_tg_state_bld_F_Yb"))
  } else if(command == driver.MAKE) {

    # silence package check
    GCAM_sector <- GCAM_fuel <- pollutant <-emissions <- state <-
      sector <- fuel <- Non.CO2 <- value <- year <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    NEI_pollutant_mapping <- get_data(all_data, "gcam-usa/emissions/NEI_pollutant_mapping")
    CEDS_GCAM_fuel <- get_data(all_data, "gcam-usa/emissions/CEDS_GCAM_fuel")
    NEI_2011_GCAM_sectors <- get_data(all_data, "gcam-usa/emissions/NEI_2011_GCAM_sectors")

    # Perform computations
    # This script assumes the data has been pre-processed. So all that needs to be done is
    # to convert to the correct sector/fuel organization and convert units

    L174.nonghg_tg_state_bld_F_Yb <- NEI_2011_GCAM_sectors %>%
      # Subset building emissions
      filter(grepl("building", GCAM_sector)) %>%
      # Format to level1 sector naming convention
      mutate(sector = gsub("building_", "", GCAM_sector)) %>%
      # using left_join because orignal CEDS fuel in NEI has one called "Process", there's no GCAM fuel corresponding to that,
      # OK to omit, missing values will be dropped later
      # TODO: check that the dropped "Process" emissions are not significant
      left_join(CEDS_GCAM_fuel, by = "CEDS_Fuel") %>%
      rename(fuel = GCAM_fuel) %>%
      rename(NEI_pollutant = pollutant) %>%
      # Match on NEI pollutants, using left_join because missing values will be produced
      # The original NEI include filterable PM2.5 and PM10, but here we only need primary ones
      # OK to omit those filterables
      left_join(NEI_pollutant_mapping, by = "NEI_pollutant") %>%
      na.omit %>%
      # Convert from short ton to Tg
      mutate(emissions = emissions / CONV_T_METRIC_SHORT / 10 ^ 6, unit = "Tg") %>%
      # generate file tibble based on standard GCAM column names, sum emissions for the same state/sector/fuel/species
      group_by(state, sector, fuel, Non.CO2) %>%
      summarise(value = sum(emissions)) %>%
      ungroup %>%
      # use long format, create column "year" with base year (2010)
      # The base-year here is specific to the USNEI data used
      # Note that we are explicitly approximating 2010 emissions using 2011 values in the NEI
      # since the NEI is only available every ~3 years
      mutate(year = gcamusa.NEI_BASE_YEAR) %>%
      select(state, sector, fuel, Non.CO2, year, value)

    #Functionalized version
    #bld_names <- grep("building", unique(NEI_2011_GCAM_sectors$GCAM_sector), value = T)
    #L174.nonghg_tg_state_bld_F_Yb <- NEI_to_GCAM(NEI_2011_GCAM_sectors, CEDS_GCAM_fuel, NEI_pollutant_mapping, bld_names) %>%
     # mutate(sector = gsub("building_", "", sector))


    L174.nonghg_tg_state_bld_F_Yb %>%
      add_title("Buildings sector non-ghg input emissions by U.S. state / sector / fuel / pollutant / year") %>%
      add_units("Tg") %>%
      add_comments("Buildings sector non-ghg input emissions by U.S. state / sector / fuel / pollutant / year") %>%
      add_legacy_name("L174.nonghg_tg_state_bld_F_Yb") %>%
      add_precursors("gcam-usa/emissions/NEI_pollutant_mapping",
                     "gcam-usa/emissions/CEDS_GCAM_fuel",
                     "gcam-usa/emissions/NEI_2011_GCAM_sectors") ->
      L174.nonghg_tg_state_bld_F_Yb

    return_data(L174.nonghg_tg_state_bld_F_Yb)
  } else {
    stop("Unknown command")
  }
}
