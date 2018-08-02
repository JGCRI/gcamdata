#' module_gcam.china_LA1321.Cement
#'
#' Allocate across the provinces national cement production, input-output cofficients, and energy inputs to cement production
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1321.out_Mt_province_cement_Yh}, \code{L1321.IO_GJkg_province_cement_F_Yh}, \code{L1321.in_EJ_province_cement_F_Y}. The corresponding file in the
#' original data system was \code{LA1321.Cement.R} (gcam-china level1).
#' @details The tables for cement production, i.e., out, and energy inputs, i.e., in, were calculated by applying state shares to national data.
#' @details The state shares were determined by the states' relative values of cement shipments.
#' @details The input-out coefficients were downscaled to the states in proportation to the national data.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author Yang Aug 2018
module_gcam.china_LA1321.Cement <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L101.inNBS_Mtce_province_S_F",
             "L1321.out_Mt_R_cement_Yh",
             "L1321.IO_GJkg_R_cement_F_Yh",
             "L1321.in_EJ_R_cement_F_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1321.out_Mt_province_cement_Yh",
             "L1321.IO_GJkg_province_cement_F_Yh",
             "L1321.in_EJ_province_cement_F_Y"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    . <- NULL   # silence package check notes

    # Load required inputs
    L1321.out_Mt_R_cement_Yh <- get_data(all_data, "L1321.out_Mt_R_cement_Yh")
    L1321.IO_GJkg_R_cement_F_Yh <- get_data(all_data, "L1321.IO_GJkg_R_cement_F_Yh")
    L1321.in_EJ_R_cement_F_Y <- get_data(all_data, "L1321.in_EJ_R_cement_F_Y")

    # Proxy for downscaling cement to provinces
    # TODO: Find a better proxy
    L101.inNBS_Mtce_province_S_F <- get_data(all_data, "L101.inNBS_Mtce_province_S_F")

    # ===================================================
    # Assigning national cement production to provinces

    L101.inNBS_Mtce_province_S_F %>%
      filter(sector == "industry" & fuel == "coal") %>%
      # Convert to province precentages
      group_by(sector, fuel, year) %>%
      summarise(sum = sum(value)) %>%
      ungroup ->
      Cement_sum

    L101.inNBS_Mtce_province_S_F %>%
      filter(sector == "industry" & fuel == "coal") %>%
      # There is NAs in Cement_sum, so use left_join replace left_join_no_match_error.
      left_join(Cement_sum,by = c("sector", "fuel", "year")) %>%
      group_by(province, sector, fuel, year) %>%
      mutate(multiplier = value / sum) %>%
      ungroup %>%
      group_by(province, fuel) %>%
      mutate(multiplier = approx_fun(year, multiplier, rule = 2), sector = "cement") %>%
      ungroup() %>%
      select(-value,-sum, -fuel) ->
      L1321.in_pct_province_cement

    # This section is calculating province-level data by multiplying the province share by the china component in the global data
    # This will generate an output table: Cement production by province / historical year
    L1321.out_Mt_R_cement_Yh %>%
      filter(GCAM_region_ID == 11) %>% # Filtering for the china component
      repeat_add_columns(tibble(province = unique(L1321.in_pct_province_cement$province))) %>% # Expanding the table to the province-level
      left_join_error_no_match(L1321.in_pct_province_cement, by = c("province", "year", "sector")) %>% # Adding the province share we calculated above
      mutate(value = value * multiplier) %>% # Multiplying the national amount with the province share
      select(province, sector, year, value) ->
      L1321.out_Mt_province_cement_Yh

    # This section is downscaling the national input/output (IO) coefficients to the province level
    # Assuming all provinces have the same IO coefficients for heat, electricity, and limestone
    # This will generate an output table: Input-output coefficients of cement production by province / fuel / historical year
    L1321.IO_GJkg_R_cement_F_Yh %>%
      filter(GCAM_region_ID == 11) %>%
      repeat_add_columns(tibble(province = unique( L1321.out_Mt_province_cement_Yh$province))) %>%
      select(province, sector, fuel, year, value) ->
      L1321.IO_GJkg_province_cement_F_Yh

    # Calculating energy inputs to cement production by province
    # Again, this section is downscaling the national data to the province level, using the province share calculated above
    # Note that this assumes the same fuel blend in all states
    # This will generate an output table: Energy inputs to cement production by province / fuel / historical year
    L1321.in_pct_province_cement %>%
      repeat_add_columns(tibble(fuel = unique(L1321.in_EJ_R_cement_F_Y$fuel))) ->
      L1321.in_pct_province_cement

    L1321.in_EJ_R_cement_F_Y %>%
      filter(GCAM_region_ID == 11) %>%
      repeat_add_columns(tibble(province = unique(L1321.in_pct_province_cement$province))) %>%
      left_join_error_no_match(L1321.in_pct_province_cement, by = c("province", "year", "sector", "fuel")) %>%
      mutate(value = multiplier * value) %>%
      select(province, sector, fuel, year, value) ->
      L1321.in_EJ_province_cement_F_Y

    # ===================================================

    L1321.out_Mt_province_cement_Yh %>%
      add_title("Cement production by province / historical year") %>%
      add_units("Mt") %>%
      add_comments("downscaling national data using province shares") %>%
      add_comments("these province shares were calculated to be proportional to the their values of cement shipments") %>%
      add_legacy_name("L1321.out_Mt_province_cement_Yh") %>%
      add_precursors("L101.inNBS_Mtce_province_S_F", "L1321.out_Mt_R_cement_Yh") ->
      L1321.out_Mt_province_cement_Yh

    L1321.IO_GJkg_province_cement_F_Yh %>%
      add_title("Input-output coefficients of cement production by province / input / historical year") %>%
      add_units("GJ/kg and kg/kg") %>%
      add_comments("downscaling national data assuming the same IO coefficients for each respective fuel") %>%
      add_legacy_name("L1321.IO_GJkg_province_cement_F_Yh") %>%
      add_precursors("L101.inNBS_Mtce_province_S_F", "L1321.IO_GJkg_R_cement_F_Yh") ->
      L1321.IO_GJkg_province_cement_F_Yh

    L1321.in_EJ_province_cement_F_Y %>%
      add_title("Energy inputs to cement production by province / fuel / historical year") %>%
      add_units("EJ/yr") %>%
      add_comments("downscaling national data using state shares") %>%
      add_comments("these state shares were calculated to be proportional to the their values of cement shipments") %>%
      add_legacy_name("L1321.in_EJ_province_cement_F_Y") %>%
      add_precursors("L101.inNBS_Mtce_province_S_F", "L1321.in_EJ_R_cement_F_Y") %>%
      add_flags(FLAG_PROTECT_FLOAT) ->
      L1321.in_EJ_province_cement_F_Y

    return_data(L1321.out_Mt_province_cement_Yh, L1321.IO_GJkg_province_cement_F_Yh, L1321.in_EJ_province_cement_F_Y)
  } else {
    stop("Unknown command")
  }
}
