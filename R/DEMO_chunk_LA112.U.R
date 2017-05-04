#' module_energy_LA112.U_DEMO
#'
#' Assigning global uranium supply curve to GCAM_region_ID 1
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
#' @author Author name(s)

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

    # ===================================================
    # Creating a tibble of global uranium supply curves
    tibble(GCAM_region_ID = 1,
      resource = A12.U_curves$resource,
      subresource = A12.U_curves$subresource,
      grade = A12.U_curves$grade,
      extractioncost = A12.U_curves$extractioncost,
      available = A12.U_curves$available) %>%
      mutate(DEMO_VALUE = energy.DEMO_CONSTANT) ->
      L112.RsrcCurves_Mt_R_U

    # Adding DEMO_OPPS constant to global uranium supply curve
    if(OLD_DATA_SYSTEM_BEHAVIOR) {
      L112.RsrcCurves_Mt_R_U$DEMO_OOPS <- 1
    } else {
      L112.RsrcCurves_Mt_R_U$DEMO_OOPS <- 2  # fixed from old data system
    }

    # Assigning global uranium supply curve to GCAM_region_ID
    L112.RsrcCurves_Mt_R_U %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") ->
      L112.RsrcCurves_Mt_R_U

    # ===================================================

    # Produce outputs
    L112.RsrcCurves_Mt_R_U %>%
      add_title(" Uranium resource curves by GCAM region") %>%
      add_units("Unit = Mt") %>%
      add_precursors("L113.RsrcCurves_EJ_R_MSW", "common/GCAM_region_names", "energy/A12.U_curves") %>%
      add_legacy_name("L112.RsrcCurves_Mt_R_U_DEMO") ->
      L112.RsrcCurves_Mt_R_U_DEMO

    return_data(L112.RsrcCurves_Mt_R_U_DEMO)
  } else {
    stop("Unknown command")
  }
}



