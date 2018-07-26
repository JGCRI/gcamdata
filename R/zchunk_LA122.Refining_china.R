#' module_gcam.china_LA122.Refining
#'
#' Downscales crude oil, corn ethanol, and biodiesel refining inputs and outputs to province-level.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L122.in_EJ_province_refining_F}, \code{L122.out_EJ_province_refining_F}. The corresponding file in the
#' original data system was \code{LA122.Refining.R} (gcam-china level1).
#' @details Downscales crude oil, corn ethanol, and biodiesel refining inputs and outputs to province-level.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author Yang Jul 2018
module_gcam.china_LA122.Refining <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/province_names_mappings",
             FILE = "gcam-china/biofuel_MT_province_F",
             "L122.in_EJ_R_refining_F_Yh",
             "L122.out_EJ_R_refining_F_Yh",
             "L101.inNBS_Mtce_province_S_F"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L122.in_EJ_province_refining_F",
             "L122.out_EJ_province_refining_F"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    GCAM_region_ID <- sector <- fuel <- year <- value <- province <- value.x <- value.y <- fuel.x  <- NULL

    # Load required inputs
    province_names_mappings   <- get_data(all_data, "gcam-china/province_names_mappings")
    L122.in_EJ_R_refining_F_Yh <- get_data(all_data, "L122.in_EJ_R_refining_F_Yh")
    L122.out_EJ_R_refining_F_Yh <- get_data(all_data, "L122.out_EJ_R_refining_F_Yh")
    L101.inNBS_Mtce_province_S_F <- get_data(all_data, "L101.inNBS_Mtce_province_S_F")
    biofuel_MT_province_F <- get_data(all_data, "gcam-china/biofuel_MT_province_F")

    # ===================================================
    # CRUDE OIL REFINING
    # NOTE: using CESYc crude oil input to refineries as basis for allocation of crude oil refining to provinces
    # Crude oil consumption by industry is the energy used at refineries (input - output)

    # Calculate the percentages of oil consumption in each province
   L101.inNBS_Mtce_province_S_F %>%
      filter(sector == "refinery",
             fuel == "crude oil") %>%
      mutate(sector = "oil refining") %>%
      replace_na(list(value = 0)) %>%
      group_by(year) %>%
      # province percentage of total in each year
      mutate(value = value / sum(value)) %>%
      ungroup() %>%
      group_by(province, sector, fuel) %>%
      mutate(value = as.numeric(value), value = approx_fun(year, value, rule = 2)) %>%
      ungroup() ->
      L122.pct_province_cor

    # Crude oil refining output by province
    # Apportion the national total to the provinces
    L122.pct_province_cor %>%
      left_join_error_no_match(filter(L122.out_EJ_R_refining_F_Yh, GCAM_region_ID == 11 & sector == "oil refining"),
                by = c("sector", "year")) %>%
      # province output value = province proportion * national output value
      mutate(value = value.x * value.y) %>%
      select(province, sector, fuel = fuel.x, year, value) ->
      L122.out_EJ_province_cor

    # Inputs to crude oil refining - same method of portional allocations, but with multiple fuels
    # Oil refining input fuels
    L122.in_EJ_R_refining_F_Yh %>%
      filter(sector == "oil refining") %>%
      select(fuel) %>%
      distinct() ->
      oil_input_fuels

    # Repeat province proportions for all fuels in oil refining sector
    L122.pct_province_cor %>%
      select(-fuel) %>%
      repeat_add_columns(oil_input_fuels) ->
      L122.pct_province_cor_repF

    # Calculate province oil input values
    L122.pct_province_cor_repF %>%
      left_join_error_no_match(filter(L122.in_EJ_R_refining_F_Yh, GCAM_region_ID == 11), by = c("sector", "fuel", "year"))  %>%
      # province input value = province proportion * national input value
      mutate(value = value.x * value.y) %>%
      select(province, sector, fuel, year, value) ->
      L122.in_EJ_province_cor_F

    # BIOMASS LIQUIDS
    # Downscale ethanol production to province

    # Calculate the percentages of corn ethanol consumption in each province
    biofuel_MT_province_F %>%
      gather(year, value, -province, -fuel) %>%
      filter(fuel == "ethanol") %>%
      mutate(year = as.numeric(year)) %>%
      complete(nesting(fuel, province), year = HISTORICAL_YEARS) %>%
      group_by(province, fuel) %>%
      mutate(value = as.numeric(value), value = approx_fun(year, value, rule = 1)) %>%
      ungroup() %>%
      mutate(fuel = 'corn', sector = "corn ethanol") %>%
      # ensure all provinces are in the data.frame even thugh they will just be NA
      complete(sector, fuel, province = province_names_mappings$province, year = HISTORICAL_YEARS) %>%
      group_by(year) %>%
      mutate(value = value / sum(value, na.rm=T)) %>%
      ungroup() %>%
      replace_na(list(value = 0)) ->
      L122.pct_province_btle

    # Corn ethanol output by province
    L122.pct_province_btle %>%
      left_join_error_no_match(filter(L122.in_EJ_R_refining_F_Yh, GCAM_region_ID == 11 & sector == "corn ethanol" & fuel == "corn"),
                by = c("sector", "year")) %>%
      # province output value = province proportion * national output value
      mutate(value = value.x * value.y) %>%
      select(province, sector, fuel = fuel.x, year, value) ->
      L122.out_EJ_province_btle

    # Corn ethanol inputs by province and fuel: Repeat percentage-wise table by number of fuel inputs
    # Corn ethanol input fuels
    L122.in_EJ_R_refining_F_Yh %>%
      filter(sector == "corn ethanol") %>%
      select(fuel) %>%
      distinct() ->
      corneth_input_fuels

    # Repeat province proportions for all fuels used in corn ethanol sector
    L122.pct_province_btle %>%
      select(-fuel) %>%
      repeat_add_columns(corneth_input_fuels) %>%
      # Corn ethanol inputs by province
      left_join_error_no_match(filter(L122.in_EJ_R_refining_F_Yh, GCAM_region_ID == 11), by = c("sector", "fuel", "year")) %>%
      # province input value = province proportion * national input value
      mutate(value = value.x * value.y) %>%
      select(province, sector, fuel, year, value) ->
      L122.in_EJ_province_btle_F


    # Biodiesel output by province
    # NOTE: SEDS does not cover biodiesel; using a separate EIA database for disaggregating this to provinces

    # Build table of percentages by historical year
    biofuel_MT_province_F %>%
      gather_years() %>%
      filter(fuel == "biodiesel") %>%
      complete(nesting(fuel, province), year = HISTORICAL_YEARS) %>%
      group_by(province, fuel) %>%
      mutate(value = approx_fun(year, value, rule = 1)) %>%
      ungroup() %>%
      # ensure all provinces are in the data.frame even thugh they will just be NA
      mutate(fuel = 'biomass oil', sector = "biodiesel") %>%
      complete(sector, fuel, province = province_names_mappings$province, year = HISTORICAL_YEARS) %>%
      group_by(year) %>%
      mutate(value = value / sum(value, na.rm=T)) %>%
      ungroup() %>%
      replace_na(list(value = 0)) ->
      L122.pct_province_btlbd


    # Apportion to the provinces
    L122.pct_province_btlbd %>%
      left_join_error_no_match(filter(L122.in_EJ_R_refining_F_Yh, GCAM_region_ID == 11 & sector == "biodiesel"), by = c("sector", "year", "fuel")) %>%
      # province output value = province proportion * national output value
      mutate(value = value.x * value.y) %>%
      select(province, sector, year, value) ->
      L122.out_EJ_province_btlbd


    # Biodiesel inputs by province and fuel
    # Biodiesel input fuels
    L122.in_EJ_R_refining_F_Yh %>%
      filter(sector == "biodiesel") %>%
      select(fuel) %>%
      distinct() ->
      biodiesel_input_fuels

    # Repeat province proportions for all fuels used in biodiesel sector
    L122.pct_province_btlbd %>%
      select(-fuel) %>%
      repeat_add_columns(biodiesel_input_fuels) %>%
      # Biodiesel inputs by province
      left_join_error_no_match(filter(L122.in_EJ_R_refining_F_Yh, GCAM_region_ID == 11), by = c("sector", "fuel", "year"))  %>%
      # province input value = province proportion * national input value
      mutate(value = value.x * value.y) %>%
      select(province, sector, fuel, year, value) ->
      L122.in_EJ_province_btlbd_F

    # Bind the tables of inputs and outputs of all refineries by province in the base years
    L122.in_EJ_province_refining_F <- bind_rows(L122.in_EJ_province_cor_F, L122.in_EJ_province_btle_F, L122.in_EJ_province_btlbd_F)

    L122.out_EJ_province_refining_F <- bind_rows( L122.out_EJ_province_cor, L122.out_EJ_province_btle, L122.out_EJ_province_btlbd)
    # ===================================================
    # Produce outputs
    L122.in_EJ_province_refining_F %>%
      add_title("Refinery energy inputs by province, sector, and fuel") %>%
      add_units("EJ") %>%
      add_comments("Crude oil, corn ethanol, and biodiesel input values apportioned to provinces") %>%
      add_legacy_name("L122.in_EJ_province_refining_F") %>%
      add_precursors("L101.inNBS_Mtce_province_S_F",
                     "L122.in_EJ_R_refining_F_Yh",
                     "gcam-china/biofuel_MT_province_F",
                     "gcam-china/province_names_mappings") %>%
      add_flags(FLAG_PROTECT_FLOAT) ->
      L122.in_EJ_province_refining_F

    L122.out_EJ_province_refining_F %>%
      add_title("Refinery output by province and sector") %>%
      add_units("EJ") %>%
      add_comments("Crude oil, corn ethanol, and biodiesel output values apportioned to provinces") %>%
      add_legacy_name("L122.out_EJ_province_refining_F") %>%
      add_precursors("L101.inNBS_Mtce_province_S_F",
                     "L122.out_EJ_R_refining_F_Yh",
                     "gcam-china/biofuel_MT_province_F",
                     "gcam-china/province_names_mappings") %>%
      # The difference is very small.
      add_flags(FLAG_PROTECT_FLOAT, FLAG_SUM_TEST) ->
      L122.out_EJ_province_refining_F

    return_data(L122.in_EJ_province_refining_F, L122.out_EJ_province_refining_F)
  } else {
    stop("Unknown command")
  }
}
