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
#' @details Buildings sector non-ghg input emissions by U.S. state / sector / fuel / pollutant / year.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YO July 2018
module_gcamusa_LB174.nonghg_bld <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/gcam-usa-emission/NEI_pollutant_mapping",
             FILE = "gcam-usa/CEDS_GCAM_fuel",
             FILE = "gcam-usa/gcam-usa-emission/NEI_2011_GCAM_sectors"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L174.nonghg_tg_state_bld_F_Yb"))
  } else if(command == driver.MAKE) {

    # silence package check
    GCAM_sector <- GCAM_fuel <- pollutant <-emissions <- state <-
      sector <- fuel <- Non.CO2 <- value <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    NEI_pollutant_mapping <- get_data(all_data, "gcam-usa/gcam-usa-emission/NEI_pollutant_mapping")
    CEDS_GCAM_fuel <- get_data(all_data, "gcam-usa/CEDS_GCAM_fuel")
    NEI_2011_GCAM_sectors <- get_data(all_data, "gcam-usa/gcam-usa-emission/NEI_2011_GCAM_sectors")


    # Perform computations
    # This script assumes the data has been pre-processed. So all that needs to be done is
    # to convert to the correct sector/fuel organization and convert units

    L174.nonghg_tg_state_bld_F_Yb <- NEI_2011_GCAM_sectors %>%
      # Subset building emissions
      filter(grepl("building", GCAM_sector)) %>%
      # Format to level1 sector naming convention
      mutate(sector = gsub("building_", "", GCAM_sector)) %>%
      # GCAM fuels; using
      # left_join becuase missing matching will be generated and filtered out in the next step
      left_join(CEDS_GCAM_fuel, by = "CEDS_Fuel") %>%
      rename(fuel = GCAM_fuel) %>%
      # GCAM pollutants (filter out missing values)
      rename(NEI_pollutant = pollutant) %>%
      # left_join becuase missing matching will be generated and filtered out in the next step
      left_join(NEI_pollutant_mapping, by = "NEI_pollutant") %>%
      # MISSING VALUES: PM filterable. Not needed bc have filt+cond. OK to omit
      na.omit %>%
      # Convert from short ton to Tg
      mutate(emissions = emissions / CONV_T_METRIC_SHORT / 10 ^ 6, unit = "Tg") %>%
      # Organize
      group_by(state, sector, fuel, Non.CO2) %>%
      summarise(value = sum(emissions)) %>%
      ungroup %>%
      mutate(year = 2010) %>%
      select(state, sector, fuel, Non.CO2, year, value)

    L174.nonghg_tg_state_bld_F_Yb %>%
      add_title("Buildings sector non-ghg input emissions by U.S. state / sector / fuel / pollutant / year") %>%
      add_units("Tg") %>%
      add_comments("Buildings sector non-ghg input emissions by U.S. state / sector / fuel / pollutant / year") %>%
      add_legacy_name("L174.nonghg_tg_state_bld_F_Yb") %>%
      add_precursors("gcam-usa/gcam-usa-emission/NEI_pollutant_mapping",
                     "gcam-usa/CEDS_GCAM_fuel",
                     "gcam-usa/gcam-usa-emission/NEI_2011_GCAM_sectors") ->
      L174.nonghg_tg_state_bld_F_Yb

    return_data(L174.nonghg_tg_state_bld_F_Yb)
  } else {
    stop("Unknown command")
  }
}
