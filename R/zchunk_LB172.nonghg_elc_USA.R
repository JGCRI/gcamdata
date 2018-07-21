#' module_gcamusa_LB172.nonghg_elc
#'
#' Base-year and future year electricity non-ghg  emissions by U.S. state / fuel / pollutant / year
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L172.nonghg_tg_state_elec_F_Yb}, \code{L172.nonghg_tgej_state_elec_F_Yf}. The corresponding file in the
#' original data system was \code{LB172.nonghg_elc_USA.R} (gcam-usa level1).
#' @details This chunk has two parts: first, it isolates electric nonghg emissions by states from NEI 2011, second, it processes future year electric emission factors from EPA to future GCAM modeling years by fuel
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YO July 2018
module_gcamusa_LB172.nonghg_elc <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/emissions/CEDS_GCAM_fuel",
             FILE = "gcam-usa/emissions/EPA_state_egu_emission_factors_ktPJ",
             FILE = "gcam-usa/emissions/NEI_pollutant_mapping",
             FILE = "gcam-usa/emissions/NEI_2011_GCAM_sectors",
             FILE = "gcam-usa/states_subregions"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L172.nonghg_tg_state_elec_F_Yb", "L172.nonghg_tgej_state_elec_F_Yf"))
  } else if(command == driver.MAKE) {


    ## Silence package check.
    GCAM_sector <- GCAM_fuel <- pollutant <- emissions <- value <- state_name <-
      year <- value <- Non.CO2 <- palette <- fuel <- sector <- state <- variable <-
      value <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    CEDS_GCAM_fuel <- get_data(all_data, "gcam-usa/emissions/CEDS_GCAM_fuel")
    NEI_pollutant_mapping <- get_data(all_data, "gcam-usa/emissions/NEI_pollutant_mapping")
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    NEI_2011_GCAM_sectors <- get_data(all_data, "gcam-usa/emissions/NEI_2011_GCAM_sectors")
    EPA_state_egu_emission_factors_ktPJ <- get_data(all_data, "gcam-usa/emissions/EPA_state_egu_emission_factors_ktPJ")

    # Perform computations

    # Input-based emissions for all plants existing in the base year
    # L172.nonghg_tg_state_elc_F_Yb: Electricity non-ghg input emissions by fuel and U.S. state in the final base year

    L172.nonghg_tg_state_elec_F_Yb <- NEI_2011_GCAM_sectors %>%
      # Subset electricity emissions
      filter(GCAM_sector == "elec_heat") %>%
      # GCAM fuel
      left_join_error_no_match(CEDS_GCAM_fuel, by = "CEDS_Fuel") %>%
      rename(fuel = GCAM_fuel, NEI_pollutant = pollutant) %>%
      # Match on NEI pollutants, using left_join because missing values will be produced
      # The original NEI include filterable PM2.5 and PM10, but here we only need primary ones
      # OK to omit those filterables
      left_join(NEI_pollutant_mapping, by = "NEI_pollutant") %>%
      na.omit %>%
      # Convert from short ton to Tg
      mutate(emissions = emissions / CONV_T_METRIC_SHORT / 10 ^ 6, unit = "Tg") %>%
      # aggreate by GCAM sector, fuel and non.CO2 species with standard GCAM column names
      rename(sector = GCAM_sector) %>%
      group_by(state, sector, fuel, Non.CO2) %>%
      summarise(value = sum(emissions)) %>%
      # this is for the base year, previously the column name is X2010 as wide format, here
      # we create a new column called year, with value as 2010 (the base year)
      mutate(year = 2010) %>%
      ungroup

    # Add additional Non.CO2 species for NOx_Coal, SO2_Coal, NOx_ELEC, SO2_ELEC for the base year
    # These species will be used in the Cross State Air Pollutantion Rule (CSAPR) constraints in policy files
    # CSAPR constrains NOx and SO2 emissions only from electric sector, so we need to create new species for them,
    # but their values are the same as NOx and SO2 in electric sector and fuel type
    # 1) Add N0x_Coal
    L172.NOx_Coal_Yb <- L172.nonghg_tg_state_elec_F_Yb %>%
      filter(fuel == "coal" & Non.CO2 == "NOx") %>%
      mutate(Non.CO2 = "NOx_Coal")
    # 2) Add S02_Coal
    L172.SO2_Coal_Yb <- L172.nonghg_tg_state_elec_F_Yb %>%
      filter(fuel == "coal" & Non.CO2 == "SO2") %>%
      mutate(Non.CO2 = "SO2_Coal")
    # 3) Add NOx_ELEC
    L172.NOx_ELEC_Yb <- L172.nonghg_tg_state_elec_F_Yb %>%
      filter(Non.CO2 == "NOx") %>%
      mutate(Non.CO2 = "NOx_ELEC")
    # 4) Add SO2_ELEC
    L172.SO2_ELEC_Yb <- L172.nonghg_tg_state_elec_F_Yb %>%
      filter(Non.CO2 == "SO2") %>%
      mutate(Non.CO2 = "SO2_ELEC")
    # Add these sudo species into L172 main data
    L172.nonghg_tg_state_elec_F_Yb <- bind_rows(L172.nonghg_tg_state_elec_F_Yb,
                                                L172.NOx_Coal_Yb, L172.SO2_Coal_Yb,
                                                L172.NOx_ELEC_Yb, L172.SO2_ELEC_Yb)


    # Input-based emission factors in the future years, use data from EPA-ORD
    # There are emission factors for any new power plants
    # L172.nonghg_tgej_state_elec_F_Yf: Electricity non-co2 emissions coefficients by fuel input and U.S. state in future model years

    L172.nonghg_tgej_state_elec_F_Yf_prior2025 <- EPA_state_egu_emission_factors_ktPJ %>%
      # Convert to long format
      gather(variable, value, -state_name, -fuel) %>%
      separate(variable, into = c("year", "Non.CO2"), sep = "_") %>%
      mutate(year = as.numeric(year)) %>%
      # NOTE: for now change oil to refined liquids
      mutate(fuel = gsub("oil", "refined liquids", fuel)) %>%
      # state code & select relevant columns
      left_join_error_no_match(states_subregions %>% select(state, state_name),
                by = c("state_name")) %>%
      mutate(sector = "elec_heat") %>%
      select(state, sector, fuel, Non.CO2, year, value) %>%
      # filter only future model years prior to 2025, since we want explicit emission factors
      # in 2015, 2020 and 2025, values after 2025 added below
      # INPUT_EMISSION_YEARS is defined as 2015, 2020 and 2025
      filter(year %in% gcamusa.ELEC_INPUT_EMISSION_YEARS)

    # Add remaining future years, keeping them constant at 2025 levels
    # (data in current file is constant after 2025)
    L172.nonghg_tgej_state_elec_F_Yf_post2025 <- L172.nonghg_tgej_state_elec_F_Yf_prior2025 %>%
      filter(year == 2025) %>%
      select(-year) %>%
      # EXTRA_EMISSION_YEARS is defined as 2030 to 2100 with 5-year step
      repeat_add_columns(tibble("year" =  gcamusa.ELEC_EXTRA_EMISSION_YEARS))

    # combine together
    L172.nonghg_tgej_state_elec_F_Yf <- bind_rows(L172.nonghg_tgej_state_elec_F_Yf_prior2025,
                                              L172.nonghg_tgej_state_elec_F_Yf_post2025) %>%
      na.omit

    # Add specific Non.CO2 species for NOx_Coal, SO2_Coal, NOx_ELEC, SO2_ELEC for future years
    # These species will be used in the Cross State Air Pollutantion Rule (CSAPR) constraints in policy files
    # CSAPR constains NOx and SO2 emissions only from electric sector, so we need to create new species for them,
    # but their values NOx and SO2 in electric sector and fuel type
    # 1) Add N0x_Coal
    L172.NOx_Coal_Yf <- L172.nonghg_tgej_state_elec_F_Yf %>%
      filter(fuel == "coal" & Non.CO2 == "NOx") %>%
      mutate(Non.CO2 = "NOx_Coal")
    # 2) Add S02_Coal
    L172.SO2_Coal_Yf <- L172.nonghg_tgej_state_elec_F_Yf %>%
      filter(fuel == "coal" & Non.CO2 == "SO2") %>%
      mutate(Non.CO2 = "SO2_Coal")
    # 3) Add NOx_ELEC
    L172.NOx_ELEC_Yf <- L172.nonghg_tgej_state_elec_F_Yf %>%
      filter(Non.CO2 == "NOx") %>%
      mutate(Non.CO2 = "NOx_ELEC")
    # 4) Add SO2_ELEC
    L172.SO2_ELEC_Yf <- L172.nonghg_tgej_state_elec_F_Yf %>%
      filter(Non.CO2 == "SO2") %>%
      mutate(Non.CO2 = "SO2_ELEC")
    # Add these sudo species into L172 main data
    L172.nonghg_tgej_state_elec_F_Yf <- bind_rows(L172.nonghg_tgej_state_elec_F_Yf,
                                                  L172.NOx_Coal_Yf, L172.SO2_Coal_Yf,
                                                  L172.NOx_ELEC_Yf, L172.SO2_ELEC_Yf)


    L172.nonghg_tg_state_elec_F_Yb %>%
      add_title("Base-year electricity non-ghg input emissions") %>%
      add_units("Tg") %>%
      add_comments("Base-year electricity non-ghg input emissions by U.S. state / fuel / pollutant / year") %>%
      add_legacy_name("LB172.nonghg_elc_USA") %>%
      add_precursors("gcam-usa/emissions/CEDS_GCAM_fuel", "gcam-usa/emissions/NEI_pollutant_mapping",
                     "gcam-usa/emissions/NEI_2011_GCAM_sectors") ->
      L172.nonghg_tg_state_elec_F_Yb

    L172.nonghg_tgej_state_elec_F_Yf %>%
      add_title("Future year electricity non-co2 input emissions coefficients") %>%
      add_units("Tg/EJ") %>%
      add_comments("Future year electricity non-co2 input emissions coefficients by U.S. state / fuel / pollutant / year") %>%
      add_legacy_name("LB172.nonghg_elc_USA") %>%
      add_precursors("gcam-usa/emissions/EPA_state_egu_emission_factors_ktPJ", "gcam-usa/states_subregions") ->
      L172.nonghg_tgej_state_elec_F_Yf

    return_data(L172.nonghg_tg_state_elec_F_Yb, L172.nonghg_tgej_state_elec_F_Yf )
  } else {
    stop("Unknown command")
  }
}
