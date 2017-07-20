#' module_emissions_L111.nonghg_en_R_S_T_Y
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L111.nonghg_tg_R_en_S_F_Yh}, \code{L111.nonghg_tgej_R_en_S_F_Yh}. The corresponding file in the
#' original data system was \code{L111.nonghg_en_R_S_T_Y.R} (emissions level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_emissions_L111.nonghg_en_R_S_T_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "emissions/EDGAR/EDGAR_sector",
             FILE = "emissions/mappings/EPA_tech",
             FILE = "emissions/mappings/GCAM_sector_tech",
             "L101.in_EJ_R_en_Si_F_Yh",
             "L101.so2_tgej_USA_en_Sepa_F_Yh",
             "L101.co_tgej_USA_en_Sepa_F_Yh",
             "L101.nox_tgej_USA_en_Sepa_F_Yh",
             "L101.voc_tgej_USA_en_Sepa_F_Yh",
             "L101.nh3_tgej_USA_en_Sepa_F_Yh",
             FILE = "emissions/EDGAR/EDGAR_SO2",
             FILE = "emissions/EDGAR/EDGAR_CO",
             FILE = "emissions/EDGAR/EDGAR_NOx",
             FILE = "emissions/EDGAR/EDGAR_NMVOC",
             FILE = "emissions/EDGAR/EDGAR_NH3"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L111.nonghg_tg_R_en_S_F_Yh",
             "L111.nonghg_tgej_R_en_S_F_Yh"))
  } else if(command == driver.MAKE) {

    sector <- fuel <- service <- IPCC_Annex <- World_Region <- ISO_A3 <- Name <- IPCC <- IPCC_description <-
      Gas <- agg_sector <- EDGAR_agg_sector <- Sector <- supplysector <- subsector <- stub.technology <-
      EDGAR_emissions <- region <- emissions <- year <- iso <- awb <- Non.CO2 <- GCAM_region_ID <-
      EPA_Category <- technology <- value <- EPA_agg_sector <- EPA_agg_fuel <- EPA_agg_fuel_ghg <-
      RCP_agg_sector <- BCOC_agg_sector <- BCOC_agg_fuel <- EPA_MACC_Sector <- IIASA_sector <-
      EPA_sector <- MAC_type1 <- Gas <- HFC_PFC <- GWP <- country_name <- region_GCAM3 <-. <-
      NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    EDGAR_sector <- get_data(all_data, "emissions/EDGAR/EDGAR_sector")
    EPA_tech <- get_data(all_data, "emissions/mappings/EPA_tech")
    GCAM_sector_tech <- get_data(all_data, "emissions/mappings/GCAM_sector_tech")
    L101.co_tgej_USA_en_Sepa_F_Yh <- get_data(all_data, "L101.co_tgej_USA_en_Sepa_F_Yh")
    L101.so2_tgej_USA_en_Sepa_F_Yh <- get_data(all_data, "L101.so2_tgej_USA_en_Sepa_F_Yh")
    L101.nox_tgej_USA_en_Sepa_F_Yh <- get_data(all_data, "L101.nox_tgej_USA_en_Sepa_F_Yh")
    L101.voc_tgej_USA_en_Sepa_F_Yh <- get_data(all_data, "L101.voc_tgej_USA_en_Sepa_F_Yh")
    L101.nh3_tgej_USA_en_Sepa_F_Yh <- get_data(all_data, "L101.nh3_tgej_USA_en_Sepa_F_Yh")

    get_data(all_data, "L101.in_EJ_R_en_Si_F_Yh") %>%
      gather(year, value, -GCAM_region_ID, -fuel, -technology, -sector) %>%
      mutate(year = as.integer(substr(year, 1, 4))) ->
      L101.in_EJ_R_en_Si_F_Yh
    get_data(all_data, "emissions/EDGAR/EDGAR_SO2") %>%
      gather(year, value, -`IPCC-Annex`, -`World Region`, -ISO_A3, -Name, -IPCC, -IPCC_description) %>%
      mutate(year = as.integer(substr(year, 1, 4))) ->
      EDGAR_SO2
    get_data(all_data, "emissions/EDGAR/EDGAR_CO") %>%
      gather(year, value, -`IPCC-Annex`, -`World Region`, -ISO_A3, -Name, -IPCC, -IPCC_description) %>%
      mutate(year = as.integer(substr(year, 1, 4))) ->
      EDGAR_CO
    get_data(all_data, "emissions/EDGAR/EDGAR_NOx") %>%
      gather(year, value, -`IPCC-Annex`, -`World Region`, -ISO_A3, -Name, -IPCC, -IPCC_description) %>%
      mutate(year = as.integer(substr(year, 1, 4))) ->
      EDGAR_NOx
    get_data(all_data, "emissions/EDGAR/EDGAR_NMVOC") %>%
      gather(year, value, -`IPCC-Annex`, -`World Region`, -ISO_A3, -Name, -IPCC, -IPCC_description) %>%
      mutate(year = as.integer(substr(year, 1, 4))) ->
      EDGAR_NMVOC
    get_data(all_data, "emissions/EDGAR/EDGAR_NH3") %>%
      gather(year, value, -`IPCC-Annex`, -`World Region`, -ISO_A3, -Name, -IPCC, -IPCC_description) %>%
      mutate(year = as.integer(substr(year, 1, 4))) ->
      EDGAR_NH3

    # ===================================================

    # First, add gas name and bind all dataframes together.

    L101.so2_tgej_USA_en_Sepa_F_Yh$Non.CO2 <- "SO2"
    L101.co_tgej_USA_en_Sepa_F_Yh$Non.CO2 <- "CO"
    L101.nox_tgej_USA_en_Sepa_F_Yh$Non.CO2 <- "NOx"
    L101.voc_tgej_USA_en_Sepa_F_Yh$Non.CO2 <- "NMVOC"

    L101.nh3_tgej_USA_en_Sepa_F_Yh %>%
      mutate(Non.CO2 = "NH3") %>%
      bind_rows(L101.so2_tgej_USA_en_Sepa_F_Yh, L101.co_tgej_USA_en_Sepa_F_Yh,
                L101.nox_tgej_USA_en_Sepa_F_Yh, L101.voc_tgej_USA_en_Sepa_F_Yh, .)  %>%
      group_by(Non.CO2, sector, fuel) %>%
      # Rename columns to match other tables.
      rename(EPA_agg_sector = sector) %>%
      rename(EPA_agg_fuel = fuel) ->
      L111.nonghg_tgej_USA_en_Sepa_F_Yh.tmp1

    # Computed unscaled emissions by country and technology.
    # Then, add non-ghg gases for combination of sector, fuel, and technology.

    L101.in_EJ_R_en_Si_F_Yh %>%
      # Renmae columns for joining.
      rename(xyear = year) %>%
      rename(energy = value)  %>%
      # Keep NAs.
      left_join(select(GCAM_sector_tech, sector, fuel, technology, EPA_agg_sector, EPA_agg_fuel),
                                by =c("sector", "fuel", "technology"))  %>%
      repeat_add_columns(tibble(Non.CO2 = emissions.NONGHG_GASES)) ->
      L111.nonghg_tg_R_en_Si_F_Yh.tmp1

    # Match in emissions factors
    # Compute unscaled emissions

    # Aggregate by EDGAR sector and region
    L111.nonghg_tg_R_en_Si_F_Yh.tmp1 %>%
      rename(year = xyear) %>%
      # Keep NAs for now.
      left_join(L111.nonghg_tgej_USA_en_Sepa_F_Yh.tmp1, by = c("Non.CO2", "EPA_agg_sector", "EPA_agg_fuel", "year"))  %>%
      rename(emfact = value) %>%
      mutate(epa_emissions = energy * emfact)  %>%
      na.omit() %>%
      left_join(select(GCAM_sector_tech, sector, fuel, EDGAR_agg_sector),
                                by =c("sector", "fuel")) ->
      L111.nonghg_tg_R_en_Si_F_Yh.tmp1

    # Create column of total EPA emissions by EDGAR sector and region
    L111.nonghg_tg_R_en_Si_F_Yh.tmp1 %>%
      group_by(GCAM_region_ID,Non.CO2, EDGAR_agg_sector, year) %>%
      summarise(EPA_emissions = sum(epa_emissions)) ->
      L111.nonghg_tg_R_en_Sedgar_Yh.tmp1

    # Compute EDGAR emissions by region and sector.

    EDGAR_SO2$Non.CO2 <- "SO2"
    EDGAR_CO$Non.CO2 <- "CO"
    EDGAR_NOx$Non.CO2 <- "NOx"
    EDGAR_NH3$Non.CO2 <- "NH3"

    EDGAR_NMVOC %>%
      mutate(Non.CO2 = "NMVOC") %>%
      bind_rows(EDGAR_SO2, EDGAR_CO, EDGAR_NOx,., EDGAR_NH3) %>%
      # To match the old code we want EDGAR years (1971-2008) and 1970.
      filter(year, year %in% emissions.EDGAR_YEARS_PLUS) %>%
      left_join(select(EDGAR_sector, agg_sector), by = c("IPCC")) ->
      testfp
    #   rename(EDGAR_agg_sector = agg_sector) %>%
    #   standardize_iso(col = "ISO_A3") %>%
    #   change_iso_code('rou', 'rom')  %>%
    #   # Switch Romania iso code to its pre-2002 value
    #   left_join(select(iso_GCAM_regID, GCAM_region_ID, iso), by = "iso") ->
    #   L111.EDGAR
    #
    # L111.EDGAR %>%
    #   spread(year,value) ->
    #   testlong
    #
    # # Create new table for international shipping & aviation emissions only.
    # L111.EDGAR %>%
    #   filter(iso %in% c("sea","air")) %>%
    #   select(-IPCC, -IPCC_description, -`World Region`, -iso, -Name, -GCAM_region_ID, -`IPCC-Annex`) %>%
    #   na.omit() %>%
    #   group_by(EDGAR_agg_sector, Non.CO2) ->
    #   L111_EDGAR_intl
    #
    # # Drop unnecessary columns, aggregate by region, and group by region, sector, NONGHG, and year.
    # L111.EDGAR %>%
    #   select(-IPCC, -IPCC_description, -`World Region`, -iso, -Name, -`IPCC-Annex`) %>%
    #   na.omit() %>%
    #   # filter(year,year %in% emissions.EDGAR_YEARS) %>%
    #   group_by(GCAM_region_ID,Non.CO2,EDGAR_agg_sector, year) %>%
    #   summarize(value = sum(value)) %>%
    #   filter(year,year %in% emissions.EDGAR_YEARS)  %>%
    #   na.omit ->
    #   L111.EDGAR.tmp1
    #
    # # Scale EPA emissions by tech to match EDGAR totals
    # # First compute scalers
    # L111.nonghg_tg_R_en_Sedgar_Yh.tmp1 %>%
    #   left_join(L111.EDGAR.tmp1, by = c("GCAM_region_ID", "Non.CO2", "EDGAR_agg_sector", "year"))  %>%
    #   rename(EDGAR_emissions = value)  %>%
    #   mutate(scaler = EDGAR_emissions / EPA_emissions /1000.0) ->
    #   L111.emiss_scaler
    #
    # # Now, scale EPA emissions
    # L111.nonghg_tg_R_en_Si_F_Yh.tmp1 %>%
    #   left_join(L111.emiss_scaler, by=c("GCAM_region_ID","Non.CO2","EDGAR_agg_sector","year")) %>%
    #   mutate(emissions = epa_emissions * scaler) %>%
    #   replace_na(list(EDGAR_emissions = 0))  %>%
    #   select(-EDGAR_emissions, -EPA_emissions) ->
    #   test3
    #
    # test3 %>%
    #   filter(EDGAR_agg_sector %in% c("trn_intl_ship", "trn_intl_air")) ->
    #   L111.nonghg_tg_R_en_Si_F_Yh_intl.tmp1
    #
    # test3 %>%
    #   filter(!EDGAR_agg_sector %in% c("trn_intl_ship", "trn_intl_air")) ->
    #   L111.nonghg_tg_R_en_Si_F_Yh_dom.tmp1
    # ===================================================

    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L111.nonghg_tg_R_en_S_F_Yh") %>%
      add_precursors("common/iso_GCAM_regID",
                     "emissions/EDGAR/EDGAR_sector",
                     "emissions/mappings/EPA_tech",
                     "emissions/mappings/GCAM_sector_tech",
                     "L101.in_EJ_R_en_Si_F_Yh",
                     "L101.so2_tgej_USA_en_Sepa_F_Yh",
                     "L101.co_tgej_USA_en_Sepa_F_Yh",
                     "L101.nox_tgej_USA_en_Sepa_F_Yh",
                     "L101.voc_tgej_USA_en_Sepa_F_Yh",
                     "L101.nh3_tgej_USA_en_Sepa_F_Yh",
                     "emissions/EDGAR/EDGAR_SO2",
                     "emissions/EDGAR/EDGAR_CO",
                     "emissions/EDGAR/EDGAR_NOx",
                     "emissions/EDGAR/EDGAR_NMVOC",
                     "emissions/EDGAR/EDGAR_NH3") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L111.nonghg_tg_R_en_S_F_Yh
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L111.nonghg_tgej_R_en_S_F_Yh") %>%
      add_precursors("common/iso_GCAM_regID",
                     "emissions/EDGAR/EDGAR_sector",
                     "emissions/mappings/EPA_tech",
                     "emissions/mappings/GCAM_sector_tech",
                     "L101.in_EJ_R_en_Si_F_Yh",
                     "L101.so2_tgej_USA_en_Sepa_F_Yh",
                     "L101.co_tgej_USA_en_Sepa_F_Yh",
                     "L101.nox_tgej_USA_en_Sepa_F_Yh",
                     "L101.voc_tgej_USA_en_Sepa_F_Yh",
                     "L101.nh3_tgej_USA_en_Sepa_F_Yh",
                     "emissions/EDGAR/EDGAR_SO2",
                     "emissions/EDGAR/EDGAR_CO",
                     "emissions/EDGAR/EDGAR_NOx",
                     "emissions/EDGAR/EDGAR_NMVOC",
                     "emissions/EDGAR/EDGAR_NH3") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L111.nonghg_tgej_R_en_S_F_Yh

    return_data(L111.nonghg_tg_R_en_S_F_Yh, L111.nonghg_tgej_R_en_S_F_Yh)
  } else {
    stop("Unknown command")
  }
}

