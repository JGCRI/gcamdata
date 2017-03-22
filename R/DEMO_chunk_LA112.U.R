#' module_energy_LA112.U_DEMO
#'
#' Build the GCAM uranium energy resource curves.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L112.RsrcCurves_Mt_R_U}. The corresponding file in the
#' original data system was \code{LA112.U.R} (energy level1).
#' @details Currently not built up from inventory data; just using GCAM 3.0 values
#' These were not disaggregated to regions in GCAM 3.0.
#' @importFrom assertthat assert_that
#' @importFrom tibble tibble
#' @import dplyr
#' @importFrom tidyr gather spread
#' @author BBL March 2017
#' @export
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
    # DEMO: these are file inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A12.U_curves <- get_data(all_data, "energy/A12.U_curves")
    # DEMO: this data is produced elsewhere in the data system. It's guaranteed
    # that we won't be called until this is available
    L113.RsrcCurves_EJ_R_MSW <- get_data(all_data, "L113.RsrcCurves_EJ_R_MSW")

    # 2a. Uranium supply curves
    # Currently not built up from inventory data; just using GCAM 3.0 values
    #These were not disaggregated to regions in GCAM 3.0. Keeping this convention for now.
    #    printlog( "NOTE: Assigning global uranium supply curve to GCAM_region_ID 1")
    tibble(GCAM_region_ID = 1,
           resource = A12.U_curves$resource,
           subresource = A12.U_curves$subresource,
           grade = A12.U_curves$grade,
           extractioncost = A12.U_curves$extractioncost,
           available = A12.U_curves$available ) %>%

      # step 2
      mutate(DEMO_VALUE = energy.ENERGY_DEMO_CONSTANT) ->

      L112.RsrcCurves_Mt_R_U

    # Realized that old step 3 wasn't correct. New behavior does xxxxxxxxx.
    if(OLD_DATA_SYSTEM_BEHAVIOR) {
      L112.RsrcCurves_Mt_R_U$DEMO_OOPS <- 1
    } else {
      L112.RsrcCurves_Mt_R_U$DEMO_OOPS <- 2
    }

    # step 4: KVC is a big fathead who is too picky about documentation
    L112.RsrcCurves_Mt_R_U %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") %>%

    # 2b. Historical uranium prices (currently assumed at global level, so no level 1 processing necessary)

    # Produce outputs
      add_title("Uranium resource curves by GCAM region") %>%
      add_units("Mt") %>%
      add_comments("Currently not built up from inventory data; just using GCAM 3.0 values") %>%
      add_comments("These were not disaggregated to regions in GCAM 3.0. Keeping this convention for now.") %>%
      add_precursors("common/GCAM_region_names", "energy/A12.U_curves", "L113.RsrcCurves_EJ_R_MSW") %>%
      # typical flags, but there are others--see `constants.R`
      add_legacy_name("L112.RsrcCurves_Mt_R_U_DEMO") ->
      L112.RsrcCurves_Mt_R_U_DEMO

    return_data(L112.RsrcCurves_Mt_R_U_DEMO)
  } else {
    stop("Unknown command")
  }
}



