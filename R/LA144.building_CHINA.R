# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcam.china_LA144.Building_CHINA
#'
#' Downscaling each province and sector's shares of  building energy use by fuel
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L142.in_EJ_province_bld_F_U}, \code The corresponding file in the
#' original data system was \code{LA142.Building.R} (gcam-china level1).
#' @details Buildings sector energy consumption
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter group_by mutate select summarise
#' @importFrom tidyr gather spread
#' @author BY January 2020

module_gcam.china_LA144.Building <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L142.in_EJ_R_bld_F_Yh",
             "L144.flsp_bm2_R_res_Yh",
             "L144.flsp_bm2_R_comm_Yh",
             FILE="gcam-china/province_names_mappings",
             FILE="gcam-china/cR_BldS_F_U_share",
             FILE="gcam-china/floorspace_m2_province_Yh",
             "L100.Pop_thous_province",
             "L101.inNBS_Mtce_province_S_F"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c())
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    L142.in_EJ_R_bld_F_Yh <- get_data(all_data, "L142.in_EJ_R_bld_F_Yh")
    L144.flsp_bm2_R_res_Yh <- get_data(all_data, "L144.flsp_bm2_R_res_Yh")
    L144.flsp_bm2_R_comm_Yh <- get_data(all_data, "L144.flsp_bm2_R_comm_Yh")
    province_names_mappings <- get_data(all_data, "gcam-china/province_names_mappings")
    cR_BldS_F_U_share <- get_data(all_data, "gcam-china/cR_BldS_F_U_share")
    flsp_m2pc_province_Yh <- get_data(all_data, "gcam-china/floorspace_m2_province_Yh")
    urban_pop_share_province_Yh <- get_data(all_data, "gcam-china/urban_pop_share_province_Yh")
    L100.Pop_thous_province <- get_data(all_data, "L100.Pop_thous_province")
    L101.inNBS_Mtce_province_S_F <- get_data(all_data, "L101.inNBS_Mtce_province_S_F")

    #L101.inNBS_Mtce_province_S_F <- readdata( "GCAMCHINA_LEVEL1_DATA", "L101.inNBS_Mtce_province_S_F", na.strings="NA" )

    # Load required inputs

    # Silence package checks

    # ===================================================
    return_data()
  }
  else {
    stop("Unknown command")
  }
}
