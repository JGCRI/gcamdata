#' module_gcam.china_L210.Resources_china
#'
#' GCAM-CHINA resource market information, prices, TechChange parameters, and supply curves.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L210.RenewRsrc_CHINA},\code{L210.UnlimitRsrc_CHINA},\code{L210.UnlimitRsrc_limestone_CHINA},
#' \code{L210.UnlimitRsrcPrice_CHINA}, \code{L210.UnlimitRsrcPrice_limestone_CHINA},\code{L210.SmthRenewRsrcCurves_wind_CHINA},
#' The corresponding file in the
#' original data system was \code{L210.resources_CHINA.R} (gcam-china level2).
#' @details GCAM-China resource market information, prices, TechChange parameters, and supply curves.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author BY June 2019

module_gcam.china_L210.Resources_china <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/province_names_mappings",
             FILE = "gcam-china/wind_potential_province",
             "L1321.out_Mt_province_cement_Yh",
             "L1231.out_EJ_province_elec_F_tech",
             "L210.RenewRsrc",
             "L210.UnlimitRsrc",
             "L210.UnlimitRsrcPrice",
             "L210.SmthRenewRsrcCurves_wind"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L210.RenewRsrc_CHINA",
             "L210.UnlimitRsrc_CHINA",
             "L210.UnlimitRsrc_limestone_CHINA",
             "L210.UnlimitRsrcPrice_CHINA",
             "L210.UnlimitRsrcPrice_limestone_CHINA",
             "L210.SmthRenewRsrcCurves_wind_CHINA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    curve.exponent <- maxResource <- maxSubResource <- mid.price <-
      region <- renewresource <- smooth.renewable.subresource <-
      unlimited.resource <- year.fillout <- province <- . <- NULL

    # Load required inputs
    province_names_mappings <- get_data(all_data, "gcam-china/province_names_mappings")
    wind_potential_province <- get_data( all_data, "gcam-china/wind_potential_province" )
    L1321.out_Mt_province_cement_Yh <- get_data(all_data, "L1321.out_Mt_province_cement_Yh")
    L1231.out_EJ_province_elec_F_tech <- get_data(all_data, "L1231.out_EJ_province_elec_F_tech")
    L210.RenewRsrc <- get_data(all_data, "L210.RenewRsrc")
    L210.UnlimitRsrc <- get_data(all_data, "L210.UnlimitRsrc")
    L210.UnlimitRsrcPrice <- get_data(all_data, "L210.UnlimitRsrcPrice")
    L210.SmthRenewRsrcCurves_wind <- get_data(all_data, "L210.SmthRenewRsrcCurves_wind")
    # ===================================================
    cement_provinces <- unique( L1321.out_Mt_province_cement_Yh$province )

    L1231.out_EJ_province_elec_F_tech %>%
      filter(fuel == "geothermal", (year == 2010 & value == 0)) %>%
      select(province) %>%
      rename(region = province) %>%
      mutate(renewresource = "geothermal") ->
      no_geo_provinces_resource

    # L210.RenewRsrc_CHINA: renewable resource info in the provinces
    L210.RenewRsrc_CHINA <- L210.RenewRsrc %>%
      filter(region == "China",
             renewresource %in% gcamchina.PROVINCE_RENEWABLE_RESOURCES) %>%
      write_to_all_provinces(LEVEL2_DATA_NAMES[["RenewRsrc"]], gcamchina.PROVINCES_ALL) %>%
      # Remove geothermal from provinces that don't have it
      anti_join(no_geo_provinces_resource, by = c("region", "renewresource")) %>%
      mutate(market = if_else(renewresource != "onshore wind resource", "China", region))

    # L210.UnlimitRsrc_CHINA: unlimited resource info in the provinces
    # TODO: If needed, add in capacity factor (from old data system, seems to be 0.3 for solar, 0 for limestone)
    L210.UnlimitRsrc_CHINA <- L210.UnlimitRsrc %>%
      filter(region == "China",
             unlimited.resource %in% gcamchina.PROVINCE_UNLIMITED_RESOURCES) %>%
      write_to_all_provinces(LEVEL2_DATA_NAMES[["UnlimitRsrc"]], gcamchina.PROVINCES_ALL)

    L210.UnlimitRsrc_limestone_CHINA <- L210.UnlimitRsrc_CHINA %>%
      filter(unlimited.resource == "limestone",
             region %in% cement_provinces)

    L210.UnlimitRsrc_CHINA <- L210.UnlimitRsrc_CHINA %>%
      filter(unlimited.resource != "limestone")

    # L210.UnlimitRsrcPrice_CHINA: unlimited resource prices in the provinces
    L210.UnlimitRsrcPrice_CHINA <- L210.UnlimitRsrcPrice %>%
      filter(region == "China",
             unlimited.resource %in% gcamchina.PROVINCE_UNLIMITED_RESOURCES) %>%
      write_to_all_provinces(LEVEL2_DATA_NAMES[["UnlimitRsrcPrice"]], gcamchina.PROVINCES_ALL)

    L210.UnlimitRsrcPrice_limestone_CHINA <- L210.UnlimitRsrcPrice_CHINA %>%
      filter(unlimited.resource == "limestone",
             region %in% cement_provinces)

    L210.UnlimitRsrcPrice_CHINA <- L210.UnlimitRsrcPrice_CHINA %>%
      filter(unlimited.resource != "limestone")

    # TODO: L210.SmthRenewRsrcTechChange_CHINA (look at old data system code chunk) can be added back in when we have distributed_solar added by province

    # L210.SmthRenewRsrcCurves_wind_CHINA: wind resource curves in the provinces
    L210.SmthRenewRsrcCurves_wind_CHINA <- L210.SmthRenewRsrcCurves_wind %>%
      filter(region == "China") %>%
      repeat_add_columns(tibble(province = gcamchina.PROVINCES_noHKMC)) %>%
      left_join_error_no_match(province_names_mappings, by = "province") %>%
      select(-maxSubResource, -mid.price, -curve.exponent) %>%
      # Add in new maxSubResource, mid.price, and curve.exponent from wind_potential_province
      left_join_error_no_match(wind_potential_province, by = c("province.name")) %>%
      # Convert wind_potential_province units from 2007$/kWh to 1975$/GJ
      mutate(mid.price = mid.price * gdp_deflator(1975, 2007) / CONV_KWH_GJ) %>%
      select(region = province, renewresource, smooth.renewable.subresource, year.fillout,
             maxSubResource = maxResource, mid.price, curve.exponent)

    # ===================================================

    # Produce outputs
    L210.RenewRsrc_CHINA %>%
      add_title("Renewable resource info in the provinces") %>%
      add_units("NA") %>%
      add_comments("L210.RenewRsrc filtered and written to all provinces") %>%
      add_legacy_name("L210.RenewRsrc_CHINA") %>%
      add_precursors("L210.RenewRsrc", "L1231.out_EJ_province_elec_F_tech") ->
      L210.RenewRsrc_CHINA

    L210.UnlimitRsrc_CHINA %>%
      add_title("Unlimited resource info in the provinces") %>%
      add_units("NA") %>%
      add_comments("L210.UnlimitRsrc filtered and written to all provinces") %>%
      add_legacy_name("L210.UnlimitRsrc_CHINA") %>%
      add_precursors("L210.UnlimitRsrc") ->
      L210.UnlimitRsrc_CHINA

    L210.UnlimitRsrc_limestone_CHINA %>%
      add_title("Limestone info in the provinces") %>%
      add_units("NA") %>%
      add_comments("L210.UnlimitRsrc filtered and written to all provinces") %>%
      add_legacy_name("L210.UnlimitRsrc_limestone_CHINA") %>%
      add_precursors("L210.UnlimitRsrc", "L1321.out_Mt_province_cement_Yh") ->
      L210.UnlimitRsrc_limestone_CHINA

    L210.UnlimitRsrcPrice_CHINA %>%
      add_title("Unlimited resource prices in the provinces") %>%
      add_units("1975$/GJ") %>%
      add_comments("L210.UnlimitRsrcPrice filtered and written to all provinces") %>%
      add_legacy_name("L210.UnlimitRsrcPrice_CHINA") %>%
      add_precursors("L210.UnlimitRsrcPrice") ->
      L210.UnlimitRsrcPrice_CHINA

    L210.UnlimitRsrcPrice_limestone_CHINA %>%
      add_title("Limestone prices in the provinces") %>%
      add_units("1975$/kg") %>%
      add_comments("L210.UnlimitRsrcPrice filtered and written to all provinces") %>%
      add_legacy_name("L210.UnlimitRsrcPrice_limestone_CHINA") %>%
      add_precursors("L210.UnlimitRsrcPrice", "L1321.out_Mt_province_cement_Yh") ->
      L210.UnlimitRsrcPrice_limestone_CHINA

    L210.SmthRenewRsrcCurves_wind_CHINA %>%
      add_title("Wind resource curves in the provinces") %>%
      add_units("maxSubResource: EJ; mid.price: 1975$/GJ") %>%
      add_comments("L210.SmthRenewRsrcCurves_wind filtered and written to all provinces") %>%
      add_legacy_name("L210.SmthRenewRsrcCurves_wind_provinces") %>%
      add_precursors("L210.SmthRenewRsrcCurves_wind", "gcam-china/wind_potential_province", "gcam-china/province_names_mappings") ->
      L210.SmthRenewRsrcCurves_wind_CHINA

    return_data(L210.RenewRsrc_CHINA, L210.UnlimitRsrc_CHINA, L210.UnlimitRsrc_limestone_CHINA,
                L210.UnlimitRsrcPrice_CHINA, L210.UnlimitRsrcPrice_limestone_CHINA, L210.SmthRenewRsrcCurves_wind_CHINA)
  } else {
    stop("Unknown command")
  }
}
