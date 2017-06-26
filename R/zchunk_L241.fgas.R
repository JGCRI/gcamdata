#' module_emissions_L241.fgas
#'
#' Briefly describe what this chunk does. KALYN
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L241.hfc_all}, \code{L241.pfc_all}, \code{L241.hfc_future}, \code{L241.fgas_all_units}. The corresponding file in the
#' original data system was \code{L241.fgas.R} (emissions level2).
#' @details Describe in detail what this chunk does. KALYN
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017 KALYN
#' @export
module_emissions_L241.fgas <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "emissions/A_regions",
             FILE = "emissions/FUT_EMISS_GV",
             FILE = "temp-data-inject/L141.hfc_R_S_T_Yh",
             FILE = "temp-data-inject/L142.pfc_R_S_T_Yh",
             FILE = "temp-data-inject/L141.hfc_ef_R_cooling_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L241.hfc_all",
             "L241.pfc_all",
             "L241.hfc_future",
             "L241.fgas_all_units"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A_regions <- get_data(all_data, "emissions/A_regions")
    FUT_EMISS_GV <- get_data(all_data, "emissions/FUT_EMISS_GV")

    get_data(all_data, "temp-data-inject/L141.hfc_R_S_T_Yh") %>%
      # The following 2 lines of code will be removed latter when we are using the 'real' data
      gather(year, value, -GCAM_region_ID, -supplysector, -subsector, -stub.technology, -Non.CO2) %>%
      mutate(year = as.numeric(substr(year, 2, 5))) -> L141.hfc_R_S_T_Yh
    get_data(all_data, "temp-data-inject/L142.pfc_R_S_T_Yh") %>%
      # The following 2 lines of code will be removed latter when we are using the 'real' data
      gather(year, value, -GCAM_region_ID, -supplysector, -subsector, -stub.technology, -Non.CO2) %>%
      mutate(year = as.numeric(substr(year, 2, 5))) -> L142.pfc_R_S_T_Yh
    get_data(all_data, "temp-data-inject/L141.hfc_ef_R_cooling_Yh") %>%
      # The following 2 lines of code will be removed latter when we are using the 'real' data
      gather(year, value, -GCAM_region_ID, -supplysector, -subsector, -stub.technology, -Non.CO2) %>%
      mutate(year = as.numeric(substr(year, 2, 5))) -> L141.hfc_ef_R_cooling_Yh

    # ===================================================
    # HFC emissions
    # L241.hfc: F-gas emissions for technologies in all regions
    # #Interpolate and add region name
    lenght(d)

    L141.hfc_R_S_T_Yh %>%
      filter(year %in% BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(input.emissions = round(value, emissions.DIGITS)) %>%
      select(-GCAM_region_ID, -value) ->
      L241.hfc_all

    # L241.pfc: F-gas emissions for technologies in all regions
    #Interpolate and add region name
    #Because no future coefs are read in for any techs, anything that's zero in all base years can be dropped
    # STOPPED HERE TRYING TO FITURE OUT HOW THE HECK YOU DO TEH SUM OF THE ROWS!!!
     L142.pfc_R_S_T_Yh %>%
       filter(year %in% emissions.BASE_YEARS) %>%
       group_by(GCAM_region_ID, supplysector, subsector, stub.technology, Non.CO2) %>%
       spread(year, value) %>%
       filter_(emissions.BASE_YEARS != 0) %>% dim



     mutate_at(num_range(emissions.BASE_YEARS), rowsum = sum(.))
       mutate(yr_sum = su)#    mutate(row_sum = colSums( .[,names(.) %in% emissions.BASE_YEARS))))) -> kalyn
    #
    # # OKAY I HACE TO FIGURE OUT THEY BEST WYA TO FILTER THIS>>>
    #
    # group_by(GCAM_region_ID, supplysector, subsector, stub.technology, Non.CO2) %>%
    #   summarise(sum(value)) %>%
    #   filter(`sum(value)` > 0) %>%





      summarise(sum(value))
    left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>% head

    # The following 2 lines of code will be removed latter when we are using the 'real' data
    # ===================================================

    # Produce outputs
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L241.hfc_all") %>%
      add_precursors("common/GCAM_region_names", "emissions/A_regions", "emissions/FUT_EMISS_GV",
                     "temp-data-inject/L141.hfc_R_S_T_Yh", "temp-data-inject/L142.pfc_R_S_T_Yh",
                     "temp-data-inject/L141.hfc_ef_R_cooling_Yh") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L241.hfc_all

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L241.pfc_all") %>%
      add_precursors("common/GCAM_region_names", "emissions/A_regions", "emissions/FUT_EMISS_GV",
                     "temp-data-inject/L141.hfc_R_S_T_Yh", "temp-data-inject/L142.pfc_R_S_T_Yh",
                     "temp-data-inject/L141.hfc_ef_R_cooling_Yh") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L241.pfc_all

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L241.hfc_future") %>%
      add_precursors("common/GCAM_region_names", "emissions/A_regions", "emissions/FUT_EMISS_GV",
                     "temp-data-inject/L141.hfc_R_S_T_Yh", "temp-data-inject/L142.pfc_R_S_T_Yh",
                     "temp-data-inject/L141.hfc_ef_R_cooling_Yh") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L241.hfc_future

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L241.fgas_all_units") %>%
      add_precursors("common/GCAM_region_names", "emissions/A_regions", "emissions/FUT_EMISS_GV",
                     "temp-data-inject/L141.hfc_R_S_T_Yh", "temp-data-inject/L142.pfc_R_S_T_Yh",
                     "temp-data-inject/L141.hfc_ef_R_cooling_Yh") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L241.fgas_all_units

    return_data(L241.hfc_all, L241.pfc_all, L241.hfc_future, L241.fgas_all_units)
  } else {
    stop("Unknown command")
  }
}

# KALYN IN THE OG DATA SYSTEM IT LOOKS LIKE THE INDIVUDAL CSV FILES ARE COMINED INTO ONE XLS DO I NEED TO DO THAT HERE??
