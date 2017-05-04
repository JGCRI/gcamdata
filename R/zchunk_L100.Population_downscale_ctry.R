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
#' @details (1) Cleans Maddison historical population data and interpolates to country and year (1700-2010). (2) Cleans SSP population scenarios for smooth join with final base year population.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author STW May 2017

module_socioeconomics_L100.Population_downscale_ctry <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "socioeconomics/socioeconomics_ctry",
             "Maddison_population",
             FILE = "socioeconomics/SSP_database_v9",
             FILE = "socioeconomics/UN_popTot"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L100.Pop_thous_ctry_Yh",
             "L100.Pop_thous_SSP_ctry_Yfut"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    socioeconomics_ctry <- get_data(all_data, "socioeconomics/socioeconomics_ctry")
    Maddison_population <- get_data(all_data, "Maddison_population")
    SSP_database_v9 <- get_data(all_data, "socioeconomics/SSP_database_v9")
    UN_popTot <- get_data(all_data, "socioeconomics/UN_popTot")

    # ===================================================

    ## (1) Historical population by country

    # First clean up Maddison raw data -- NOTE: Maddison data are used to develop population ratios relative to 1950 to combine with UN data from 1950 onward
    pop_thous_ctry_reg <- Maddison_population %>%
      filter(!is.na(Country)) %>%  # Remove values with missing country name (all year values are also missing for these--probably just blank rows in the original data)
      rename(Maddison_ctry = Country,
             pop = value) %>%  # Change name to match socioeconomics_ctry mapping file
      left_join(socioeconomics_ctry, by = "Maddison_ctry")  # Join with iso codes

    # Second, estimate population values prior to 1950 for countries in aggregate regions. This is what we want: pop_country_t = (pop_aggregate_t / pop_aggregate_1950) * pop_country_1950
    # Generate a scalar for population in each aggregate region in 1950 (to generate the population ratios)
    agg_ratio <- pop_thous_ctry_reg %>%
      select(Maddison_ctry, year, pop) %>%
      filter(year <= socioeconomics.AGG_BASE_YEAR & (Maddison_ctry == "Total Former USSR" | Maddison_ctry == "Czechoslovakia" | Maddison_ctry == "Yugoslavia")) %>%  # Only want years prior to 1951 for the three regions
      group_by(Maddison_ctry) %>%  # Group to perform action on each aggregate region individually
      mutate(ratio = pop/pop[year == socioeconomics.AGG_BASE_YEAR]) %>%  # Create ratio of population in prior years to population in 1950
      rename(Downscale_from = Maddison_ctry) %>%  # Will match each country in the region to this ratio
      select(-pop) %>%
      left_join(filter(pop_thous_ctry_reg, year == socioeconomics.AGG_BASE_YEAR), by = "Downscale_from") %>%  # Join with the 1950 populations for each member country
      mutate(pop_scale = pop * ratio) %>%  # Create population values prior to 1950 for downscaled countries based on their 1950 populations and pop ration from aggregate regions
      rename(year = year.x) %>%  # Want to keep the scaled years
      ungroup() %>%  # Need to remove grouping so that we can keep only the variables we want and join with the primary data
      select(iso, year, pop_scale)
    # Replace Maddison NA population values for the downscaled countries with scaled values
    reg_scaled <- as_tibble(pop_thous_ctry_reg) %>%
      left_join(agg_ratio, by = c("iso", "year")) %>%
      mutate(pop = if_else((year < socioeconomics.AGG_BASE_YEAR & !is.na(pop_scale)), pop_scale, pop)) %>%  # Replace NA values for countries with downscaled population values
      filter(!is.na(iso)) %>%  # Remove rows with no iso code (generally aggregate regions)
      select(iso, year, pop)  # Keep only necessary variables

    # Third, interpolate available population data to generate values for required historical years (1700, 1750, 1800, 1850, 1900)
    hist_interp <- tibble(year = socioeconomics.MADDISON_HISTORICAL_YEARS) %>%  # Create tibble with required years (not all are in the original Maddison data)
      full_join(reg_scaled, by = "year") %>%  # Join with available population data (creates NAs for years not available in raw data)
      complete(year, nesting(iso)) %>%  # Completes tibble to include all years for all iso (creates missing values)
      arrange(iso, year) %>%  # For easier readability when checking results
      filter(!is.na(iso)) %>%  #
      group_by(iso) %>%
      mutate(pop = approx_fun(year, pop)) %>%  # Interpolate -- note that there will still be missing values for countries that do not have end values (1500, 1600, or 1700)
      mutate(pop = if_else(is.na(pop) & year == 1800, pop[year == 1820], pop))  # Replace missing 1800 values with 1820 for countries that begin in 1820

    # Fourth, for countries with any missing historic years, scale with average global population change (countries with complete observations)
    # Ratios for historic years compared to 1950 for countries with complete observations
    global_ratio <- spread(hist_interp, year, pop) %>%
      na.omit() %>%  # Find complete cases
      gather(year, pop, -iso) %>%
      mutate(year = as.integer(year)) %>%
      filter(year %in% c(socioeconomics.MADDISON_HISTORICAL_YEARS, socioeconomics.AGG_BASE_YEAR)) %>%  # Keep only the years we want
      group_by(year) %>%
      summarize(pop = sum(pop)) %>%  # Total population for complete cases
      mutate(ratio_global = pop/pop[year == socioeconomics.AGG_BASE_YEAR]) %>%  # Create ratio of population in prior years to population in 1950
      select(-pop)
    # Apply these ratios to the countries with missing values
    maddison_hist_ratio <- filter(hist_interp, year %in% c(socioeconomics.MADDISON_HISTORICAL_YEARS, socioeconomics.AGG_BASE_YEAR)) %>%  # Keep only the years we want
      full_join(global_ratio, by = "year") %>%  # Join with ratios
      group_by(iso) %>%  # For each country...
      mutate(pop = if_else(is.na(pop), (pop[year == socioeconomics.AGG_BASE_YEAR] * ratio_global), pop)) %>%  # For each country with missing population prior to 1950, multiply population in 1950 times ratio
      select(-ratio_global) %>%  # Delete global population ratios
      mutate(ratio_iso = pop/pop[year == socioeconomics.AGG_BASE_YEAR]) %>%  # Create country-level population values relative to 1950 to combine with UN population data
      select(-pop)
    # Need to create matching iso codes for three countries in UN, but not Maddison.
    # Set Serbia and Montenegro ratio equal to Serbia & Montenegro and use Indonesia population ratio for East Timor
    mne_srb_tls <- filter(maddison_hist_ratio, iso == "idn" | iso == "scg") %>%
      spread(iso, ratio_iso) %>%
      rename(mne = scg, tls = idn) %>%
      mutate(srb = mne) %>%
      gather(iso, ratio_iso, -year)
    # Combine with other ratio_iso values
    maddison_hist_ratio <- bind_rows(maddison_hist_ratio, mne_srb_tls)

    # Fifth, since UN population data are available from 1950+, use Maddison ratios to extrapolate pre-1950 values relative to UN 1950
    un_pop_clean <- UN_popTot %>%  # Clean raw UN population data
      filter(Scenario == "EST") %>%  # Historic only (not projections)
      select(-Region, -Sex, -Scenario) %>%
      rename(iso = Country, year = Year, pop = Value) %>%
      mutate(iso = tolower(iso)) %>%
      filter(year %in% socioeconomics.UN_HISTORICAL_YEARS) %>%  # Keep only the years that we are using
      mutate(iso = gsub("xea", "twn", iso))  # Correct iso code for Taiwan

    # Sixth, combine with Maddison historic ratios to get values for pre-1950 years
    L100.Pop_thous_ctry_Yh <- un_pop_clean %>%
      full_join(maddison_hist_ratio, by = c("iso", "year")) %>%
      complete(iso, nesting(year)) %>%  # Completes tibble to include all years for all iso
      group_by(iso) %>%
      mutate(pop = if_else((year %in% socioeconomics.MADDISON_HISTORICAL_YEARS), (pop[year == socioeconomics.AGG_BASE_YEAR] * ratio_iso), pop)) %>%  # Multiply pop in 1950 by ratio to get prior years' pop
      filter(iso != "scg") %>%  # "Serbia & Montenegro" separated into individual countries, do not need combined country
      mutate(pop = if_else(is.na(pop), 0, pop)) %>%  # Per old data system: At this point the remaining mismatched countries are small, so set to zero
      mutate(year = as.integer(year)) %>%  # To avoid problems later on, save years as integers
      rename(value = pop) %>%
      select(-ratio_iso)

    ## (2) SSP population projections by country

    # Final historical population from UN
    pop_final_hist <- filter(L100.Pop_thous_ctry_Yh, year == socioeconomics.FINAL_HIST_YEAR) %>%
      rename(pop_final_hist = value) %>%
      select(-year)

    # First, generate ratios of future population to base year (2010) for all SSPs. The ratios will be applied to the historical year populations so there are no jumps/inconsistencies.
    L100.Pop_thous_SSP_ctry_Yfut <- as_tibble(SSP_database_v9) %>% # Note units in SSP database are millions, but convert to thousands when we multiply by historic year
      filter(MODEL == "IIASA-WiC POP" & VARIABLE == "Population") %>%  # IIASA-WiC is the official SSP population data set
      mutate(iso = tolower(REGION), scenario = substr(SCENARIO, 1,4)) %>%
      select(-MODEL, -VARIABLE, -UNIT, -REGION, -SCENARIO) %>%
      mutate(iso = gsub("rou", "rom", iso)) %>%  # SSP uses "rou" for the iso for Romania; replace with "rom" for consistency with other data sources
      gather(year, pop, -iso, -scenario) %>%  # Long format
      mutate(year = as.integer(year), pop = as.numeric(pop)) %>%  # Clean year variable
      filter(year %in% c(socioeconomics.FINAL_HIST_YEAR, modeltime.FUTURE_YEARS)) %>% # Retain only years needed for GCAM
      group_by(scenario, iso) %>%
      mutate(ratio_iso_ssp = pop / pop[year == socioeconomics.FINAL_HIST_YEAR]) %>%  # Calculate population ratios to final historical year (2010), no units
      select(-pop) %>%
      # Second, project country population values using SSP ratios and final historical year populations.
      # Not all countries in the UN data are in SSP data. Create complete tibble with all UN countries & SSP years.
      ungroup() %>%
      complete(scenario = unique(scenario),
               year = unique(year),
               iso = unique(L100.Pop_thous_ctry_Yh$iso)) %>%
      # For these countries, the ratio will be set to 1 (per the old data system).
      mutate(ratio_iso_ssp = if_else(is.na(ratio_iso_ssp), 1, ratio_iso_ssp)) %>%
      ## Note: In the old data system, Taiwan is in this category and has constant population. Issue has been opened to deal with this later. ##
      left_join(pop_final_hist, by = "iso") %>% # Join with final historic period population
      mutate(value = pop_final_hist * ratio_iso_ssp) %>%  # Units are 1000 persons (UN 2010 value is in thousands)
      select(-pop_final_hist, -ratio_iso_ssp)

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
      add_precursors("socioeconomics/socioeconomics_ctry", "Maddison_population") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L100.Pop_thous_ctry_Yh

    tibble() %>%
      add_title("SSP population projections by country, 2010-2100") %>%
      add_units("thousand") %>%
      add_comments("Future population calculated as final historical year (2010) population times ratio of SSP future years to SSP 2010") %>%
      # add_comments("can be multiple lines") %>%
      add_legacy_name("L100.Pop_thous_SSP_ctry_Yfut") %>%
      add_precursors("socioeconomics/socioeconomics_ctry", "socioeconomics/SSP_database_v9", "socioeconomics/UN_popTot") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L100.Pop_thous_SSP_ctry_Yfut

    return_data(L100.Pop_thous_ctry_Yh, L100.Pop_thous_SSP_ctry_Yfut)
  } else {
    stop("Unknown command")
  }
}
