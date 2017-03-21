#' module_energy_LA112.U_DEMO
#'
#' Produce uranium supply curves for nuclear energy potential.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L112.RsrcCurves_Mt_R_U}. The corresponding file in the
#' original data system was \code{LA112.U.R} (energy level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom tibble tibble
#' @import dplyr
#' @importFrom tidyr gather spread
#' @author BBL
module_energy_LA112.U_DEMO <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/A12.U_curves",
             "L113.RsrcCurves_EJ_R_MSW"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L112.RsrcCurves_Mt_R_U_DEMO"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A12.U_curves <- get_data(all_data, "energy/A12.U_curves")
    L113.RsrcCurves_EJ_R_MSW <- get_data(all_data, "L113.RsrcCurves_EJ_R_MSW")

    # -----------------------------------------------------------------------------
    # 2. Perform computations
    # 2a. Uranium supply curves
    # Currently not built up from inventory data; just using GCAM 3.0 values
    #These were not disaggregated to regions in GCAM 3.0. Keeping this convention for now.
 #   printlog( "NOTE: Assigning global uranium supply curve to GCAM_region_ID 1")

    tibble(GCAM_region_ID = 1,
           resource = A12.U_curves$resource,
           subresource = A12.U_curves$subresource,
           grade = A12.U_curves$grade,
           extractioncost = A12.U_curves$extractioncost,
           available = A12.U_curves$available ) %>%
      # step 2
      mutate(DEMO_VALUE = energy.ENERGY_DEMO_CONSTANT)  ->
      L112.RsrcCurves_Mt_R_U

      # step 3
    if(OLD_DATA_SYSTEM_BEHAVIOR) {
      L112.RsrcCurves_Mt_R_U$DEMO_OOPS <- 1
    } else {
      L112.RsrcCurves_Mt_R_U$DEMO_OOPS <- 2
    }

    # 2b. Historical uranium prices (currently assumed at global level, so no level 1 processing necessary)

    # step 4
    L112.RsrcCurves_Mt_R_U %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") %>%

    # Produce outputs
      add_title("Uranium supply curves") %>%
      add_units("Mt") %>%
      add_comments("These curves determine nuclear energy generation capability") %>%
      add_comments("Currently not built up from inventory data; just using GCAM 3.0 values") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A12.U_curves") %>%
      # typical flags, but there are others--see `constants.R`
      add_legacy_name("L112.RsrcCurves_Mt_R_U_DEMO") %>%
      add_flags(FLAG_NO_XYEAR) ->
      L112.RsrcCurves_Mt_R_U_DEMO

    return_data(L112.RsrcCurves_Mt_R_U_DEMO)
  } else {
    stop("Unknown command")
  }
}



