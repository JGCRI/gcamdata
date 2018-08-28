#' module_gcam.china_LA154.Transport
#'
#' Downscale transportation energy consumption and nonmotor data to the province level, generating three ouput tables.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L154.in_EJ_province_trn_m_sz_tech_F}, \code{L154.out_mpkm_province_trn_nonmotor_Yh}, \code{L154.in_EJ_province_trn_F}. The corresponding file in the
#' original data system was \code{LA154.Transport.R} (gcam-china level1).
#' @details Transportation energy data was downscaled in proportion to NBS province-level transportation energy data
#' @details Transportation nonmotor data was downscaled in proportion to province population
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author AJS June 2017
module_gcam.china_LA154.Transport <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/trnUCD_NBS_mapping",
             "L154.in_EJ_R_trn_m_sz_tech_F_Yh",
             "L154.out_mpkm_R_trn_nonmotor_Yh",
             "L100.Pop_thous_province",
             "L101.inNBS_Mtce_province_S_F",
             "L101.NBS_use_all_Mtce"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L154.in_EJ_province_trn_m_sz_tech_F",
             "L154.out_mpkm_province_trn_nonmotor_Yh",
             "L154.in_EJ_province_trn_F"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    trnUCD_NBS_mapping <- get_data(all_data, "gcam-china/trnUCD_NBS_mapping")
    L154.in_EJ_R_trn_m_sz_tech_F_Yh <- get_data(all_data, "L154.in_EJ_R_trn_m_sz_tech_F_Yh")
    L154.out_mpkm_R_trn_nonmotor_Yh <- get_data(all_data, "L154.out_mpkm_R_trn_nonmotor_Yh")
    L100.Pop_thous_province <- get_data(all_data, "L100.Pop_thous_province")
    L101.inNBS_Mtce_province_S_F <- get_data(all_data, "L101.inNBS_Mtce_province_S_F")
    L101.NBS_use_all_Mtce <- get_data(all_data, "L101.NBS_use_all_Mtce")
    # ===================================================

      # Silence package notes
      GCAM_region_ID <- year <- value <- UCD_sector <- size.class <- UCD_technology <- UCD_fuel <- fuel <- EBProcess <- EBMaterial <-
      fuel_sector <- province <- sector <- value_national <- value_share <- pop <- value_mode <- NULL

      # Calculate the province-wise percentages for each of NBS's sector/fuel combinations that is relevant for disaggregating
      # nation-level transportation energy to the provinces

      # This starting table is transportation energy consumption by GCAM region (and other variables)
      # We will first subset this data for only the china and values that are > 0 in the historical periods

      L154.in_EJ_R_trn_m_sz_tech_F_Yh %>%
        filter(year %in% HISTORICAL_YEARS, GCAM_region_ID == 11) %>% # Filter for the china and for historical years only
        filter(value != 0) %>% # Here any rows with value of 0 will be lost, even if other years of the same group are nonzero
        # We will next reintroduce those rows using "complete" and assign those values to be 0
        complete(nesting(GCAM_region_ID, UCD_sector, mode, size.class, UCD_technology, UCD_fuel, fuel), year = HISTORICAL_YEARS, fill = list(value = 0)) %>%
        # Fuel and mode will be mapped to NBS fuel and sector
        left_join_error_no_match(trnUCD_NBS_mapping, by = c("fuel", "mode")) ->
        L154.in_EJ_CHINA_trn_m_sz_tech_F_Yh


      #next, extract the relevant NBS sector & fuel combinations from the full province database
      L101.NBS_use_all_Mtce %>%
        filter(year %in% HISTORICAL_YEARS) %>% # Ensure within historical period
        mutate(fuel_sector = paste(EBProcess, EBMaterial)) %>% # Create concatenated list in base dataframe to match the syntax of our list above
        filter(fuel_sector %in% paste(L154.in_EJ_CHINA_trn_m_sz_tech_F_Yh$EBProcess, L154.in_EJ_CHINA_trn_m_sz_tech_F_Yh$EBMaterial)) %>% # Filtering for just NBS-fuel/sector pairs
        select(province, EBProcess, EBMaterial, sector, fuel, year, value) %>%
        group_by(province, year) %>%
        # First zero out NAs in years where some values are NA but not all
        mutate(value = replace(value, is.na(value) & sum(value, na.rm = T) != 0, 0)) %>%
        ungroup %>%
        # use approx_fun rule = 2 to fill out data in years where the entire province is NA
        group_by(province, EBProcess, EBMaterial) %>%
        mutate(value = approx_fun(year, value, rule = 2)) %>%
        ungroup() ->
        L154.NBS_trn_Mtce_province

      # To calculate the province share, we need to calculate the national amount
      L154.NBS_trn_Mtce_province %>%
        group_by(EBProcess, EBMaterial, year) %>% # Dropping province
        summarise(value_national = sum(value, na.rm = T)) %>%
        ungroup() ->
        NBS_transportation_national

      # Now the province shares can be calculated by dividing the province data by the national
      L154.NBS_trn_Mtce_province %>%
        left_join_error_no_match(NBS_transportation_national, by = c("EBProcess", "EBMaterial", "year")) %>%
        mutate(value_share = value / value_national) %>% # Calculating province's share
        # NAs were introduced where national values were 0. Replace NAs with zeros.
        replace_na(list(value_share = 0)) %>%
        select(province, EBProcess, EBMaterial, year, value_share) ->
        L154.NBS_trn_share_province

      # Match these percentages into a table of all transportation technologies that will be written out
      L154.in_EJ_CHINA_trn_m_sz_tech_F_Yh %>%
        repeat_add_columns(tibble::tibble(province = gcamchina.PROVINCES)) ->
      L154.pct_province_trn_m_sz_tech_F_Yh

      # TODO: Tibet
      #L154.pct_province_trn_m_sz_tech_F_Yh <- subset( L154.pct_province_trn_m_sz_tech_F_Yh, province != "XZ" )

      L154.pct_province_trn_m_sz_tech_F_Yh %>%
        filter(year %in% HISTORICAL_YEARS) %>%
        mutate(fuel_sector = paste(EBProcess, EBMaterial)) %>%
        left_join(L154.NBS_trn_share_province, by = c("province", "EBProcess", "EBMaterial", "year")) %>%
        replace_na(list(value_share = 0)) %>%
        mutate(value = NULL) ->
        L154.pct_province_trn_m_sz_tech_F_Yh

      #Now, the full CHINA tran UCD database can be apportioned to the provinces
      # Creating the first of the three output tables
      L154.pct_province_trn_m_sz_tech_F_Yh%>%
        left_join_error_no_match(L154.in_EJ_CHINA_trn_m_sz_tech_F_Yh, by = c("year", "UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel","fuel")) %>%
        mutate(value = value * value_share) %>% # Allocating across the provinces
        select(province, UCD_sector, mode, size.class, UCD_technology, UCD_fuel, fuel, year, value) ->
        L154.in_EJ_province_trn_m_sz_tech_F

      # As a final step, aggregate by fuel and name the sector
      # This creates the second of three output tables
      L154.in_EJ_province_trn_m_sz_tech_F %>%
        group_by(province, fuel, year) %>%
        summarise(value = sum(value)) %>%
        ungroup() %>%
        mutate(sector = "transportation") %>% # Adding a column named "sector" with "transportation" as the entries
        select(province, sector, fuel, year, value) ->
        L154.in_EJ_province_trn_F

      # Apportion non-motorized energy consumption to provinces on the basis of population
      # First we will create the province shares based on population
      L100.Pop_thous_province %>%
        complete( province, year = 1971:2100) %>%
        group_by(province) %>%
        mutate(pop = approx_fun(year, pop, rule = 2)) %>%
        ungroup() %>%
        group_by(year) %>%
        summarise(value_national = sum(pop, na.rm =T)) ->
        Pop_national

      L100.Pop_thous_province %>%
        complete( province, year = 1971:2100) %>%
        group_by(province) %>%
        mutate(pop = approx_fun(year, pop, rule = 2)) %>%
        ungroup() %>%
        left_join_error_no_match(Pop_national, by = "year") %>%
        mutate(value_share = pop / value_national) %>% # Creating province share based on population
        select(province, year, value_share) ->
        Pop_province_share


        # Now we can use these shares to allocate the national data across the provinces
        L154.out_mpkm_R_trn_nonmotor_Yh %>%
          rename(value_mode = value) %>%
          filter(GCAM_region_ID == 11) %>%
          # Number of rows will change by adding provinces, so left_join_error_no_match cannot be used
          left_join(Pop_province_share, by = "year") %>%
          mutate(value = value_mode * value_share) %>% # Apportioning across the modes using the share data
          filter(year %in% HISTORICAL_YEARS) %>% # Ensuring within historical period
          select(province, mode, year, value) %>%
          mutate(year = as.integer(year)) ->
          L154.out_mpkm_province_trn_nonmotor_Yh


    # ===================================================

    L154.in_EJ_province_trn_m_sz_tech_F %>%
      add_title("Transportation energy consumption by province, sector, mode, size class, and fuel") %>%
      add_units("EJ") %>%
      add_comments("Transportation energy consumption data was downscaled to the province level using NBS province energy data") %>%
      add_legacy_name("L154.in_EJ_province_trn_m_sz_tech_F") %>%
      add_precursors("L154.in_EJ_R_trn_m_sz_tech_F_Yh", "gcam-china/trnUCD_NBS_mapping", "L101.NBS_use_all_Mtce", "L101.inNBS_Mtce_province_S_F") %>%
      add_flags(FLAG_PROTECT_FLOAT) ->
      L154.in_EJ_province_trn_m_sz_tech_F

    L154.out_mpkm_province_trn_nonmotor_Yh %>%
      add_title("Transportation non-motorized travel by mode and province") %>%
      add_units("million person-km") %>%
      add_comments("National data was allocated across the provinces in proportion to population") %>%
      add_legacy_name("L154.out_mpkm_province_trn_nonmotor_Yh") %>%
      add_precursors("L154.out_mpkm_R_trn_nonmotor_Yh", "L100.Pop_thous_province")  %>%
      add_flags(FLAG_PROTECT_FLOAT, FLAG_SUM_TEST) ->
      L154.out_mpkm_province_trn_nonmotor_Yh

    L154.in_EJ_province_trn_F %>%
      add_title("Transportation energy consumption by province and fuel") %>%
      add_units("EJ") %>%
      add_comments("Transportation energy consumption was aggregated by fuel, and the sector was named transportation") %>%
      add_legacy_name("L154.in_EJ_province_trn_F") %>%
      add_precursors("L154.in_EJ_R_trn_m_sz_tech_F_Yh", "gcam-china/trnUCD_NBS_mapping", "L101.NBS_use_all_Mtce", "L101.inNBS_Mtce_province_S_F") %>%
      add_flags(FLAG_PROTECT_FLOAT ) ->
      L154.in_EJ_province_trn_F

    return_data(L154.in_EJ_province_trn_m_sz_tech_F, L154.out_mpkm_province_trn_nonmotor_Yh, L154.in_EJ_province_trn_F)
  } else {
    stop("Unknown command")
  }
}
