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
#' @details elec sector non-ghg emissions by U.S. state / sector / fuel / pollutant / base and future year.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom dplyr %>%
#' @importFrom tidyr gather spread
#' @author YO July 2018
module_gcamusa_LB172.nonghg_elc <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/CEDS_GCAM_fuel",
             FILE = "gcam-usa/EPA_state_egu_emission_factors_ktPJ",
             FILE = "gcam-usa/gcam-usa-emission/NEI_pollutant_mapping",
             FILE = "gcam-usa/gcam-usa-emission/NEI_2011_GCAM_sectors",
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
    CEDS_GCAM_fuel <- get_data(all_data, "gcam-usa/CEDS_GCAM_fuel")
    NEI_pollutant_mapping <- get_data(all_data, "gcam-usa/gcam-usa-emission/NEI_pollutant_mapping")
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    NEI_2011_GCAM_sectors <- get_data(all_data, "gcam-usa/gcam-usa-emission/NEI_2011_GCAM_sectors")
    EPA_state_egu_emission_factors_ktPJ <- get_data(all_data, "gcam-usa/EPA_state_egu_emission_factors_ktPJ")

    # Perform computations

    ## Input-based emissions in the base year
    # L172.nonghg_tg_state_elc_F_Yb: Electricity non-ghg input emissions by fuel and U.S. state in the final base year

    L172.nonghg_tg_state_elec_F_Yb <- NEI_2011_GCAM_sectors %>%
      # Subset electricity emissions
      filter(GCAM_sector == "elec_heat") %>%
      # GCAM fuel
      left_join(CEDS_GCAM_fuel, by = "CEDS_Fuel") %>%
      rename(fuel = GCAM_fuel, NEI_pollutant = pollutant) %>%
      # Match on NEI pollutants, using left_join becuase missing values will be produced and dropped later
      left_join(NEI_pollutant_mapping, by = "NEI_pollutant") %>%
      # MISSING VALUES: PM filterable. Not needed bc have filt+cond. OK to omit
      na.omit %>%
      # Convert from short ton to Tg
      mutate(emissions = emissions / CONV_T_METRIC_SHORT / 10 ^ 6, unit = "Tg") %>%
      # Organize
      rename(sector = GCAM_sector) %>%
      group_by(state, sector, fuel, Non.CO2) %>%
      summarise(emissions = sum(emissions)) %>%
      rename(value = emissions) %>%
      mutate(year = 2010) %>%
      ungroup

    ## Add specific Non.CO2 species for NOx_Coal, SO2_Coal, NOx_ELEC, SO2_ELEC for the base year
    # These species will be used in CSAPR constrains in policy files
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


    ## Input-based emissions in the future years, use data from EPA-ORD
    # L172.nonghg_tgej_state_elec_F_Yf: Electricity non-co2 emissions coefficients by fuel input and U.S. state in future model years

    L172.nonghg_tgej_state_elec_F_Yf_prior2025 <- EPA_state_egu_emission_factors_ktPJ %>%
      # Convert to long format
      gather(variable, value, -state_name, -fuel) %>%
      separate(variable, into = c("year","Non.CO2"), sep = "_") %>%
      mutate(year = as.numeric(year)) %>%
      # NOTE: for now change oil to refined liquids
      mutate(fuel = gsub("oil","refined liquids", fuel)) %>%
      # state code & select relevant columns
      left_join(states_subregions %>% select(state, state_name),
                by = c("state_name")) %>%
      mutate(sector = "elec_heat") %>%
      select(state, sector, fuel, Non.CO2, year, value) %>%
      # filter only future model years prior to 2025
      filter(year %in% FUTURE_YEARS[1:3])

    # Add remaining future years, keeping them constant at 2025 levels
    L172.nonghg_tgej_state_elec_F_Yf_post2025 <- L172.nonghg_tgej_state_elec_F_Yf_prior2025 %>%
      filter(year == 2025) %>%
      select(-year) %>%
      repeat_add_columns(tibble("year" =  FUTURE_YEARS[4:18]))

    # combine together
    L172.nonghg_tgej_state_elec_F_Yf <- bind_rows(L172.nonghg_tgej_state_elec_F_Yf_prior2025,
                                              L172.nonghg_tgej_state_elec_F_Yf_post2025) %>%
      na.omit

    ## Add sodu Non.CO2 species for NOx_Coal, SO2_Coal, NOx_ELEC, SO2_ELEC for future years
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
      add_precursors("gcam-usa/CEDS_GCAM_fuel", "gcam-usa/gcam-usa-emission/NEI_pollutant_mapping",
                     "gcam-usa/gcam-usa-emission/NEI_2011_GCAM_sectors") ->
      L172.nonghg_tg_state_elec_F_Yb

    L172.nonghg_tgej_state_elec_F_Yf %>%
      add_title("Future year electricity non-co2 input emissions coefficients") %>%
      add_units("Tg/EJ") %>%
      add_comments("Future year electricity non-co2 input emissions coefficients by U.S. state / fuel / pollutant / year") %>%
      add_legacy_name("LB172.nonghg_elc_USA") %>%
      add_precursors("gcam-usa/EPA_state_egu_emission_factors_ktPJ", "gcam-usa/states_subregions") ->
      L172.nonghg_tgej_state_elec_F_Yf

    return_data(L172.nonghg_tg_state_elec_F_Yb, L172.nonghg_tgej_state_elec_F_Yf )
  } else {
    stop("Unknown command")
  }
}
