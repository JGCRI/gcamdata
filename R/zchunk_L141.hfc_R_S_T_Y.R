#' module_emissions_L141.hfc_R_S_T_Y
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vectdriveor of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L141.hfc_R_S_T_Yh}, \code{L141.hfc_ef_R_cooling_Yh}. The corresponding file in the
#' original data system was \code{L141.hfc_R_S_T_Y.R} (emissions level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RMH Aug 2017

module_emissions_L141.hfc_R_S_T_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "emissions/gcam_fgas_tech",
             FILE = "emissions/other_f_gases",
             FILE = "temp-data-inject/L144.in_EJ_R_bld_serv_F_Yh",
             FILE = "common/iso_GCAM_regID",
             FILE = "emissions/EDGAR/EDGAR_sector_fgas",
             FILE = "emissions/EDGAR/EDGAR_HFC125",
             FILE = "emissions/EDGAR/EDGAR_HFC134a",
             FILE = "emissions/EDGAR/EDGAR_HFC143a",
             FILE = "emissions/EDGAR/EDGAR_HFC152a",
             FILE = "emissions/EDGAR/EDGAR_HFC227ea",
             FILE = "emissions/EDGAR/EDGAR_HFC23",
             FILE = "emissions/EDGAR/EDGAR_HFC236fa",
             FILE = "emissions/EDGAR/EDGAR_HFC245fa",
             FILE = "emissions/EDGAR/EDGAR_HFC32",
             FILE = "emissions/EDGAR/EDGAR_HFC365mfc",
             FILE = "emissions/EDGAR/EDGAR_HFC43",
             FILE = "emissions/HFC_Inventory_GV"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L141.hfc_R_S_T_Yh",
             "L141.hfc_ef_R_cooling_Yh"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    gcam_fgas_tech <- get_data(all_data, "emissions/gcam_fgas_tech")
    other_f_gases <- get_data(all_data, "emissions/other_f_gases")
    L144.in_EJ_R_bld_serv_F_Yh <- get_data(all_data, "temp-data-inject/L144.in_EJ_R_bld_serv_F_Yh")  %>%
      gather(year, value, -GCAM_region_ID, -sector, -fuel, -service) %>%
      mutate(year = as.integer(substr(year, 2, 5)))
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    EDGAR_sector <- get_data(all_data, "emissions/EDGAR/EDGAR_sector_fgas")
    EDGAR_HFC125 <- get_data(all_data, "emissions/EDGAR/EDGAR_HFC125")
    EDGAR_HFC134a <- get_data(all_data, "emissions/EDGAR/EDGAR_HFC134a")
    EDGAR_HFC143a <- get_data(all_data, "emissions/EDGAR/EDGAR_HFC143a")
    EDGAR_HFC152a <- get_data(all_data, "emissions/EDGAR/EDGAR_HFC152a")
    EDGAR_HFC227ea <- get_data(all_data, "emissions/EDGAR/EDGAR_HFC227ea")
    EDGAR_HFC23 <- get_data(all_data, "emissions/EDGAR/EDGAR_HFC23")
    EDGAR_HFC236fa <- get_data(all_data, "emissions/EDGAR/EDGAR_HFC236fa")
    EDGAR_HFC245fa <- get_data(all_data, "emissions/EDGAR/EDGAR_HFC245fa")
    EDGAR_HFC32 <- get_data(all_data, "emissions/EDGAR/EDGAR_HFC32")
    EDGAR_HFC365mfc <- get_data(all_data, "emissions/EDGAR/EDGAR_HFC365mfc")
    EDGAR_HFC43 <- get_data(all_data, "emissions/EDGAR/EDGAR_HFC43")
    HFC_Inventory_GV <- get_data(all_data, "emissions/HFC_Inventory_GV")


    # Beginning processing and Mapping EDGAR HFC emissions to GCAM technologies

    #Add column with Non.CO2 gas name
    EDGAR_HFC125$Non.CO2 <- "HFC125"
    EDGAR_HFC134a$Non.CO2 <- "HFC134a"
    EDGAR_HFC143a$Non.CO2 <- "HFC143a"
    EDGAR_HFC152a$Non.CO2 <- "HFC152a"
    EDGAR_HFC227ea$Non.CO2 <- "HFC227ea"
    EDGAR_HFC23$Non.CO2 <- "HFC23"
    EDGAR_HFC236fa$Non.CO2 <- "HFC236fa"
    EDGAR_HFC245fa$Non.CO2 <- "HFC245fa"
    EDGAR_HFC32$Non.CO2 <- "HFC32"
    EDGAR_HFC365mfc$Non.CO2 <- "HFC365mfc"
    EDGAR_HFC43$Non.CO2 <- "HFC43"

    # Convert year columns to numeric values, then combine into one tibble
    # define function to convert data to numeric values
    years_to_numeric <- function(xx){
      xx[as.character(1970:2000)] <- sapply(xx[as.character(1970:2000)],as.numeric)
      return(xx)
    }

    F_gases <- list(EDGAR_HFC125, EDGAR_HFC134a, EDGAR_HFC143a,
                    EDGAR_HFC152a, EDGAR_HFC227ea, EDGAR_HFC23,
                    EDGAR_HFC236fa, EDGAR_HFC245fa, EDGAR_HFC32,
                    EDGAR_HFC365mfc, EDGAR_HFC43)

    # Use function to convert datat to numeric values and combine to one tibble
    # Map to GCAM countries and technologies
    F_gases_numeric <- lapply(F_gases, years_to_numeric)
    L141.EDGAR_HFC <- do.call(bind_rows,F_gases_numeric) %>%
      left_join_error_no_match(EDGAR_sector %>% select(-IPCC_description), by = "IPCC") %>% # Add Edgar agg_sector
      mutate(EDGAR_agg_sector = agg_sector) %>% #rename agg_sector to EDGAR_agg_sector
      mutate(iso = tolower(ISO_A3), ISO_A3 = NULL) %>% # convert to Edgar ISO
      change_iso_code('rou', 'rom') %>% # Convert Romania iso code to pre-2002 value
      left_join_error_no_match(iso_GCAM_regID,by = "iso") %>% # Map iso to GCAM region
      select(GCAM_region_ID, iso, EDGAR_agg_sector,Non.CO2,one_of(as.character(emissions.EDGAR_YEARS)))
    L141.EDGAR_hfc_R_S_T_Yh.long <- L141.EDGAR_HFC %>%
      gather(year, emissions, -GCAM_region_ID,-iso,-EDGAR_agg_sector,-Non.CO2) %>%
      group_by(GCAM_region_ID, EDGAR_agg_sector, Non.CO2,  year) %>%
      summarize(emissions = sum(emissions))

    # Map to other f-gas sector, which varies by gas
    L141.EDGAR_hfc_R_S_T_Yh_rest <- L141.EDGAR_hfc_R_S_T_Yh.long %>%
      filter(EDGAR_agg_sector != "other_f_gases")
    L141.EDGAR_hfc_R_S_T_Yh_other <- L141.EDGAR_hfc_R_S_T_Yh.long %>%
      filter(EDGAR_agg_sector == "other_f_gases") %>%
      ungroup() %>%
      select(-EDGAR_agg_sector) %>%
      left_join_error_no_match(other_f_gases, by = c('Non.CO2' = 'Gas')) %>%
      rename(EDGAR_agg_sector=Sector)
    L141.EDGAR_hfc_R_S_T_Yh.long <- bind_rows( L141.EDGAR_hfc_R_S_T_Yh_rest,
                                               L141.EDGAR_hfc_R_S_T_Yh_other ) %>%
      mutate(year = as.numeric(year))

    # Map Emissions to GCAM technologies
    L141.hfc_R_S_T_Yh.long <- gcam_fgas_tech %>%
      repeat_add_columns(tibble::tibble(GCAM_region_ID = unique(GCAM_region_names$GCAM_region_ID))) %>%
      repeat_add_columns(tibble::tibble(year = emissions.EDGAR_YEARS)) %>%
      repeat_add_columns(tibble::tibble(Non.CO2 = unique(L141.EDGAR_hfc_R_S_T_Yh.long$Non.CO2))) %>%
      left_join(L141.EDGAR_hfc_R_S_T_Yh.long,
                by = c("EDGAR_agg_sector", "GCAM_region_ID", "year", "Non.CO2")) %>% # there should be NAs in the match.Some emissions sectors have zero F gass emissions
      mutate_at(vars(),funs(replace(.,is.na(.),0)))

    # Disaggregate cooling emissions to residential and commercial sectors
    # Calculate share of res/com cooling emissions from L144.in_EJ_R_bld_serv_F_Yh
    # Apply share to HFC emissions

    # Select residential and cooling emisssions from L144.in_EJ_R_bld_serv_F_Yh
    L141.R_cooling_T_Yh.long <- L144.in_EJ_R_bld_serv_F_Yh %>%
      filter(service %in% c("comm cooling", "resid cooling") & fuel == "electricity")
    # Group by GCAM region and ID and year in new data frame (use to calculate share of total later)
    L141.R_cooling_Yh <-  L141.R_cooling_T_Yh.long %>%  group_by(GCAM_region_ID,year) %>%
      summarize(total = sum(value))
    # join in the totals we just calculated and calulate the shares
    L141.R_cooling_T_Yh.long <- left_join_error_no_match(L141.R_cooling_T_Yh.long,L141.R_cooling_Yh, by = c("GCAM_region_ID", "year")) %>%
      mutate(share = value / total ) %>%
      mutate(year = as.numeric(gsub('X','',year))) %>%
      select(GCAM_region_ID,year,service,share,value)

    # Add res/com cooling share to HFC and make adjustment
    L141.hfc_R_S_T_Yh_share<- left_join(L141.hfc_R_S_T_Yh.long,
                                        L141.R_cooling_T_Yh.long ,
                                        by = c("GCAM_region_ID", "year", "supplysector" = "service")) %>% # there should be NAs.
                                        #there are sectors that don't have emissions
      mutate_at(vars(),funs(replace(.,is.na(.),1))) %>% # replace those shares with "1"
      mutate(emissions = emissions*share)

    # add EDGAR 2008 year, as 2010 to match with Guus
    TEMP <- subset( L141.hfc_R_S_T_Yh_share, year == 2008 )
    TEMP$year <- 2010
    L141.hfc_R_S_T_Yh_share <- bind_rows( L141.hfc_R_S_T_Yh_share, TEMP )

    # process Guus inventory
    HFC_Inventory_GV <- HFC_Inventory_GV %>% mutate(Species = gsub("-","", Species)) %>%
      mutate(Species = gsub("4310mee", "43",Species)) %>%
      dplyr::rename(Guus_tot = Emissions)

    # add Guus values to HFC emissions then calculate scalar
    L141.hfc_scaler <- L141.hfc_R_S_T_Yh_share %>% group_by(year,Non.CO2) %>%
      summarize(EDGAR_tot = sum(emissions))%>%
      arrange(Non.CO2,year) %>%
      left_join(HFC_Inventory_GV, by = c(year = "Year", Non.CO2 = "Species")) %>% # some entries not in GUUS data, default to scaler =1
      mutate(scaler = Guus_tot/EDGAR_tot) %>%
      mutate_at(vars(), funs(replace(., is.na(.), 1))) %>%
      mutate_at(vars(), funs(replace(., is.infinite(.), 0))) %>%
      select(year, Non.CO2, scaler)

    # Add scalar to HFC emissions, calculate adjusted emissions
    L141.hfc_R_S_T_Yh <- L141.hfc_R_S_T_Yh_share %>% left_join(L141.hfc_scaler,
                                        by = c("year", "Non.CO2") ) %>% # there are NA's, not all sectors have emissions/scalers
      mutate(adj_emissions = emissions*scaler) %>%
      group_by(GCAM_region_ID, supplysector, subsector, stub.technology, Non.CO2, year) %>% #this takes SOOOO LONG
      summarise(adj_emissions = sum(adj_emissions, na.rm = T)) %>%
      mutate_at(vars(), funs(replace(., is.na(.), 0))) #replace NAs with zero. Some sectors have zero emissions.

    #Compute final cooling HFC emissions factors
    L141.hfc_ef_R_cooling_Yh <- L141.hfc_R_S_T_Yh %>%
      subset( supplysector %in% c( "comm cooling", "resid cooling" ) ) %>%
      left_join_error_no_match(L141.R_cooling_T_Yh.long %>% select(GCAM_region_ID, year,service,value),
                               by = c("GCAM_region_ID", "year","supplysector"="service")) %>%
      rename(energy = value) %>%
      mutate(em_fact = adj_emissions / energy) %>%
      select(GCAM_region_ID, supplysector, subsector, stub.technology, Non.CO2, year, em_fact) %>%
      mutate_at(vars(), funs(replace(., is.na(.), 0))) %>%
      rename(value = em_fact)

    # final processing - rename a "value" column
    L141.hfc_R_S_T_Yh <- L141.hfc_R_S_T_Yh %>%
      rename(value = adj_emissions)

    # Produce outputs
   L141.hfc_R_S_T_Yh %>%
      add_title("HFC emissions by region / sector / technology / gas / historical year") %>%
      add_units("Unit = Gg") %>%
      add_comments("Edgar emissions, scaled to Guus HFC inventory for residential and commercial cooling") %>%
      add_legacy_name("L141.hfc_R_S_T_Yh") %>%
      add_precursors("common/GCAM_region_names",
                     "emissions/gcam_fgas_tech",
                     "emissions/other_f_gases",
                     "temp-data-inject/L144.in_EJ_R_bld_serv_F_Yh",
                     "common/iso_GCAM_regID",
                     "emissions/EDGAR/EDGAR_sector_fgas",
                     "emissions/EDGAR/EDGAR_HFC125",
                     "emissions/EDGAR/EDGAR_HFC134a",
                     "emissions/EDGAR/EDGAR_HFC143a",
                     "emissions/EDGAR/EDGAR_HFC152a",
                     "emissions/EDGAR/EDGAR_HFC227ea",
                     "emissions/EDGAR/EDGAR_HFC23",
                     "emissions/EDGAR/EDGAR_HFC236fa",
                     "emissions/EDGAR/EDGAR_HFC245fa",
                     "emissions/EDGAR/EDGAR_HFC32",
                     "emissions/EDGAR/EDGAR_HFC365mfc",
                     "emissions/EDGAR/EDGAR_HFC43",
                     "emissions/HFC_Inventory_GV") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L141.hfc_R_S_T_Yh
    L141.hfc_ef_R_cooling_Yh %>%
      add_title("HFC emissions factors for cooling by region / sector / technology / gas / historical year") %>%
      add_units("Unit = Gg / EJ") %>%
      add_comments("HFC emissions (scaled to Guus data) divided by GCAM cooling energy use") %>%
      add_legacy_name("L141.hfc_ef_R_cooling_Yh") %>%
      add_precursors("common/GCAM_region_names",
                     "emissions/gcam_fgas_tech",
                     "emissions/other_f_gases",
                     "temp-data-inject/L144.in_EJ_R_bld_serv_F_Yh",
                     "common/iso_GCAM_regID",
                     "emissions/EDGAR/EDGAR_sector_fgas",
                     "emissions/EDGAR/EDGAR_HFC125",
                     "emissions/EDGAR/EDGAR_HFC134a",
                     "emissions/EDGAR/EDGAR_HFC143a",
                     "emissions/EDGAR/EDGAR_HFC152a",
                     "emissions/EDGAR/EDGAR_HFC227ea",
                     "emissions/EDGAR/EDGAR_HFC23",
                     "emissions/EDGAR/EDGAR_HFC236fa",
                     "emissions/EDGAR/EDGAR_HFC245fa",
                     "emissions/EDGAR/EDGAR_HFC32",
                     "emissions/EDGAR/EDGAR_HFC365mfc",
                     "emissions/EDGAR/EDGAR_HFC43",
                     "emissions/HFC_Inventory_GV") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L141.hfc_ef_R_cooling_Yh

    return_data(L141.hfc_R_S_T_Yh, L141.hfc_ef_R_cooling_Yh)
  } else {
    stop("Unknown command")
  }
}
