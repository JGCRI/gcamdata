#' module_socioeconomics_L100.Population_downscale_ctry
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L100.Pop_thous_ctry_Yh}, \code{L100.Pop_thous_SSP_ctry_Yfut}. The corresponding file in the
#' original data system was \code{L100.Population_downscale_ctry.R} (socioeconomics level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_socioeconomics_L100.Population_downscale_ctry <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "socioeconomics/socioeconomics_ctry",
             FILE = "socioeconomics/Maddison_population",
             FILE = "socioeconomics/SSP_database_v9",
             FILE = "socioeconomics/UN_popTot"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L100.Pop_thous_ctry_Yh",
             "L100.Pop_thous_SSP_ctry_Yfut"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    socioeconomics_ctry <- get_data(all_data, "socioeconomics/socioeconomics_ctry")
    Maddison_population <- get_data(all_data, "socioeconomics/Maddison_population")
    SSP_database_v9 <- get_data(all_data, "socioeconomics/SSP_database_v9")
    UN_popTot <- get_data(all_data, "socioeconomics/UN_popTot")

    # ===================================================

    ## Historical population by country

    # First, clean Maddison raw data
    pop_thous_ctry_reg <- Maddison_population %>%
      filter(!is.na(Country)) %>% # Remove values with missing country name (all year values are also missing for these--probably just blank rows in the original data)
      rename(Maddison_ctry = Country) %>% # Change name to match socioeconomics_ctry mapping file
      select(-X194) %>% # Remove blank column
      gather(year, pop, 2:194) %>% # Convert to long format
      mutate(year = as.numeric(substr(year, 2,5))) %>% #Create numeric year value
      left_join(socioeconomics_ctry, by = "Maddison_ctry") # Join with iso codes

    # Second, estimate population values prior to 1950 for countries in aggregate regions. This is what we want: pop_country_t = (pop_aggregate_t / pop_aggregate_1950) * pop_country_1950
    # Generate a scalar for population in each aggregate region in 1950 (to generate the population ratios)
    agg_ratio <- pop_thous_ctry_reg %>%
      select(Maddison_ctry, year, pop) %>%
      filter(year <= 1950 & (Maddison_ctry == "Total Former USSR" | Maddison_ctry == "Czechoslovakia" | Maddison_ctry == "Yugoslavia")) %>% # Only want years prior to 1951 for the three regions
      group_by(Maddison_ctry) %>% # Group to perform action on each aggregate region individually
      mutate(ratio = pop/pop[year == 1950]) %>% # Create ratio of population in prior years to population in 1950
      rename(Downscale_from = Maddison_ctry) %>% # Will match each country in the region to this ratio
      select(-pop) %>%
      left_join(filter(pop_thous_ctry_reg, year == 1950), by = "Downscale_from") %>% # Join with the 1950 populations for each member country
      mutate(pop_scale = pop * ratio) %>% # Create population values prior to 1950 for downscaled countries based on their 1950 populations and pop ration from aggregate regions
      rename(year = year.x) %>% # Want to keep the scaled years
      ungroup() %>% # Need to remove grouping so that we can keep only the variables we want and join with the primary data
      select(iso, year, pop_scale)
    # Replace Maddison NA population values for the downscaled countries with scaled values
    reg_scaled <- as_tibble(pop_thous_ctry_reg) %>%
      left_join(agg_ratio, by = c("iso", "year")) %>%
      mutate(pop = if_else((year < 1950 & !is.na(pop_scale)), pop_scale, pop)) %>% # Replace NA values for countries with downscaled population values
      filter(!is.na(iso)) %>% # Remove rows with no iso code (generally aggregate regions)
      select(iso, year, pop) # Keep only necessary variables

    # Third, interpolate available population data to generate values for required historical years (1700, 1750, 1800, 1850, 1900)
    hist_interp <- tibble(year = socioeconomics.MADDISON_HISTORICAL_YEARS) %>% # Create tibble with required years (not all are in the original Maddison data)
      full_join(reg_scaled, by = "year") %>% # Join with available population data (creates NAs for years not available in raw data)
      complete(year, nesting(iso)) %>% # Completes tibble to include all years for all iso (creates missing values)
      arrange(iso, year) %>% # For easier readability when checking results
      filter(!is.na(iso)) %>% #
      group_by(iso) %>%
      mutate(pop = approx_fun(year, pop)) %>% # Interpolate -- note that there will still be missing values for countries that do not have end values (1500, 1600, or 1700)
      mutate(pop = if_else((is.na(pop) & year == 1800), pop[year == 1820], pop)) # Replace missing 1800 values with 1820 for countries that begin in 1820

    # Fourth, for countries with any missing historic years, scale with average global population change (countries with complete observations)
    # Ratios for historic years compared to 1950 for countries with complete observations
    global_ratio <- spread(hist_interp, year, pop) %>%
      na.omit() %>% # Find complete cases
      gather("year", "pop", 2:196) %>%
      mutate(year = as.integer(year)) %>%
      filter(year == 1700 | year == 1750 | year == 1800 | year == 1850 | year == 1900 | year == 1950) %>% # Keep only the years we want
      group_by(year) %>%
      summarize(pop = sum(pop)) %>% # Total population for complete cases
      mutate(ratio_global = pop/pop[year == 1950]) %>% # Create ratio of population in prior years to population in 1950
      select(-pop)
    # Apply these ratios to the countries with missing values
    maddison_hist_ratio <- filter(hist_interp, year == 1700 | year == 1750 | year == 1800 | year == 1850 | year == 1900 | year == 1950 ) %>% # Keep only the years we want
      full_join(global_ratio, by = "year") %>% # Join with ratios
      group_by(iso) %>% # For each country...
      mutate(pop = if_else(is.na(pop), (pop[year == 1950] * ratio_global), pop)) %>% # For each country with missing population prior to 1950, multiply population in 1950 times ratio
      select(-ratio_global) %>% # Delete global population ratios
      mutate(ratio_iso = pop/pop[year == 1950]) %>% # Create country-level population values relative to 1950 to combine with UN population data
      select(-pop)
    # Need to create matching iso codes for three countries in UN, but not Maddison.
    # Set Serbia and Montenegro ratio equal to Serbia & Montenegro and use Indonesia population ratio for East Timor
    mne_srb_tls <- filter(maddison_hist_ratio, iso == "idn" | iso == "scg") %>%
      spread(iso, ratio_iso) %>%
      rename(mne = scg, tls = idn) %>%
      mutate(srb = mne) %>%
      gather(iso, ratio_iso, 2:4)
    # Combine with other ratio_iso values
    maddison_hist_ratio <- bind_rows(maddison_hist_ratio, mne_srb_tls)

    # Fifth, since UN population data are available from 1950+, use Maddison ratios to extrapolate pre-1950 values relative to UN 1950
    un_pop_clean <- UN_popTot %>% # Clean raw UN population data
      filter(Scenario == "EST") %>% # Historic only (not projections)
      select(-Region, -Sex, -Scenario) %>%
      rename(iso = Country, year = Year, pop = Value) %>%
      mutate(iso = tolower(iso)) %>%
      filter(year == 1950 | (year >= 1970 & year <= 2010)) %>% # Keep only the years that we are using
      mutate(iso = gsub("xea", "twn", iso)) # Correct iso code for Taiwan

    # Sixth, combine with Maddison historic ratios to get values for pre-1950 years
    L100.Pop_thous_ctry_Yh <- un_pop_clean %>%
      full_join(maddison_hist_ratio, by = c("iso", "year")) %>%
      complete(iso, nesting(year)) %>% # Completes tibble to include all years for all iso
      group_by(iso) %>%
      mutate(pop = if_else((year == 1700 | year == 1750 | year == 1800 | year == 1850 | year == 1900), (pop[year == 1950] * ratio_iso), pop)) %>% # Multiply pop in 1950 by ratio to get prior years' pop
      filter(iso != "scg") %>% # "Serbia & Montenegro" separated into individual countries, do not need combined country
      mutate(pop = if_else(is.na(pop), 0, pop)) %>% # Per old data system: At this point the remaining mismatched countries are small, so set to zero
      mutate(year = as.integer(year)) %>% # To avoid problems later on, save years as integers
      select(-ratio_iso)

    ## SSP population projections by country
    # ===================================================

    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
    tibble() %>%
      add_title("Population by country, 1700-2010") %>%
      add_units("thousand") %>%
      add_comments("Maddison population data cleaned to develop complete data for all years, (dis)aggregated to modern country boundaries") %>%
      # add_comments("can be multiple lines") %>%
      add_legacy_name("L100.Pop_thous_ctry_Yh") %>%
      add_precursors("socioeconomics_ctry", "Maddison_population") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L100.Pop_thous_ctry_Yh

    tibble() %>%
      add_title("SSP...") %>%
      add_units("thousand") %>%
      add_comments("add comments here") %>%
      # add_comments("can be multiple lines") %>%
      add_legacy_name("L100.Pop_thous_SSP_ctry_Yfut") %>%
      add_precursors("socioeconomics_ctry", "SSP_database_v9", "UN_popTot") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L100.Pop_thous_SSP_ctry_Yfut

    return_data(L100.Pop_thous_ctry_Yh, L100.Pop_thous_SSP_ctry_Yfut)
  } else {
    stop("Unknown command")
  }
}


