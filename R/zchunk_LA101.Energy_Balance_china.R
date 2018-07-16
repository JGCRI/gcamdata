#' module_gcam.china_LA101.Energy_Balance
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L101.NBS_use_all_Mtce}, \code{L101.inNBS_Mtce_province_S_F}. The corresponding file in the
#' original data system was \code{LA101.Energy_Balance.R} (gcam-china level1).
#' @details See above
#' @importFrom assertthat assert_that
#' @importFrom tibble tibble
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread fill
#' @author Yang July 2018
#' @export

module_gcam.china_LA101.Energy_Balance <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/province_names_mappings",
             FILE = "gcam-china/NBS_CESY_process",
             FILE = "gcam-china/NBS_CESY_material",
             FILE = "gcam-china/en_balance_Mtce_Yh_province",
		  	     FILE = "gcam-china/Tibet_share",
			       FILE = "gcam-china/tibet_shares_mappings"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L101.NBS_use_all_Mtce",
             "L101.inNBS_Mtce_province_S_F"))
  } else if(command == driver.MAKE) {

   # silence package check.

  all_data <- list(...)[[1]]

    # Load required inputs
	  province_names_mappings <- get_data(all_data, "gcam-china/province_names_mappings")
    NBS_CESY_process <- get_data(all_data, "gcam-china/NBS_CESY_process")
    NBS_CESY_material <- get_data(all_data, "gcam-china/NBS_CESY_material")
    en_balance_Mtce_Yh_province <- get_data(all_data, "gcam-china/en_balance_Mtce_Yh_province")
    Tibet_share  <- get_data(all_data, "gcam-china/Tibet_share")
    tibet_shares_mappings <- get_data(all_data, "gcam-china/tibet_shares_mappings")

    # Perform computations
    # Removing the whole China category, and matching in intermediate fuel and sector names
    # The energy balance calls Tibet by it's alternative name Xizang, we will need to switch
    # it for the mappings to work
    en_balance_Mtce_Yh_province %>%
      subset(province.name != "China" & year %in% historical_years) -> L101.NBS_use_all_Mtce

    #Aggregating NBS province energy data by GCAM sector and fuel
    L101.NBS_use_all_Mtce[L101.NBS_use_all_Mtce$province.name == "Xizang", "province.name"] <- "Tibet" %>%
      map_province_name(province_names_mappings, "province", TRUE) %>%
      merge(NBS_CESY_process, all.x=TRUE) %>%
      merge(NBS_CESY_material, all.x=TRUE) %>%
      subset(!is.na( sector ) & !is.na( fuel ) ) %>%
      group_by(province,year,sector,fuel) %>%


    L101.inNBS_Mtce_province_S_F <- aggregate( value ~ province + year + sector + fuel, L101.inNBS_Mtce_province_S_F, FUN=sum)

    # cast years accross for easier reading
    L101.NBS_use_all_Mtce$year <- paste0( 'X', L101.NBS_use_all_Mtce$year )
    L101.inNBS_Mtce_province_S_F$year <- paste0( 'X', L101.inNBS_Mtce_province_S_F$year )
    L101.NBS_use_all_Mtce <- dcast( L101.NBS_use_all_Mtce, ... ~ year, value.name="value" )
    L101.inNBS_Mtce_province_S_F <- dcast( L101.inNBS_Mtce_province_S_F, ... ~ year, value.name="value" )

    # Produce outputs
    L101.NBS_use_all_Mtce %>%
      add_title("NBS china energy statistical yearbook by sector / fuel / year") %>%
      add_units("Unit = Mtce") %>%
      add_comments("historical years") %>%
      add_legacy_name("L101.NBS_use_all_Mtce") %>%
      add_precursors("gcam-china/province_names_mappings",
                     "gcam-china/NBS_CESY_process",
                     "gcam-china/NBS_CESY_material",
                     "gcam-china/en_balance_Mtce_Yh_province",
                     "gcam-china/Tibet_share",
                     "gcam-china/tibet_shares_mappings") ->
      L101.NBS_use_all_Mtce

    L101.inNBS_Mtce_province_S_F %>%
      add_title("NBS china energy statistical yearbook by GCAM sector / fuel / year") %>%
      add_units("Unit = Mtce") %>%
      add_comments("historical years") %>%
      add_legacy_name("L101.inNBS_Mtce_province_S_F") %>%
      add_precursors("gcam-china/province_names_mappings",
                     "gcam-china/NBS_CESY_process",
                     "gcam-china/NBS_CESY_material",
                     "gcam-china/en_balance_Mtce_Yh_province",
                     "gcam-china/Tibet_share",
                     "gcam-china/tibet_shares_mappings") ->
    L101.inNBS_Mtce_province_S_F

    return_data(L101.NBS_use_all_Mtce, L101.inNBS_Mtce_province_S_F)
  } else {
    stop("Unknown command")
  }
}
