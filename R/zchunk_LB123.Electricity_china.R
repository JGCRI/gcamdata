#' module_gcam.china_LB123.Electricity
#'
#' Electricity sector inputs and outputs, and electricity ownuse.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L123.in_EJ_province_elec_F}, \code{L123.out_EJ_province_elec_F}, \code{L123.in_EJ_province_ownuse_elec}, \code{L123.out_EJ_province_ownuse_elec}. The corresponding file in the
#' original data system was \code{LB123.Electricity.R} (gcam-china level1).
#' @details Compute electricity sector inputs and outputs, and electricity ownuse.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author Liu August 2018
module_gcam.china_LB123.Electricity <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/CPSY_GWh_province_F_elec_out",
             FILE = "gcam-china/province_names_mappings",
             "L123.in_EJ_R_elec_F_Yh",
             "L123.out_EJ_R_elec_F_Yh",
             "L126.in_EJ_R_elecownuse_F_Yh",
             "L126.out_EJ_R_elecownuse_F_Yh",
             "L101.inNBS_Mtce_province_S_F",
             "L132.out_EJ_province_indchp_F"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L123.in_EJ_province_elec_F",
             "L123.out_EJ_province_elec_F",
             "L123.in_EJ_province_ownuse_elec",
             "L123.out_EJ_province_ownuse_elec"))
  } else if(command == driver.MAKE) {

    # =============================================================================
    fuel <- value <- State <- . <- value.x <- value.y <- sector <- NULL     # silence package check.
    all_data <- list(...)[[1]]
    HISTORICAL_YEARS_c = c(HISTORICAL_YEARS, 2011, 2012)
    # -----------------------------------------------------------------------------
    # 1.Load required inputs
    province_names_mappings     <- get_data(all_data, "gcam-china/province_names_mappings")
    CPSY_GWh_province_F_elec_out <- get_data(all_data, "gcam-china/CPSY_GWh_province_F_elec_out")
    L101.inNBS_Mtce_province_S_F <- get_data(all_data, "L101.inNBS_Mtce_province_S_F")
    L123.in_EJ_R_elec_F_Yh <- get_data(all_data, "L123.in_EJ_R_elec_F_Yh")
    L123.out_EJ_R_elec_F_Yh <- get_data(all_data, "L123.out_EJ_R_elec_F_Yh")
    L126.in_EJ_R_elecownuse_F_Yh <- get_data(all_data, "L126.in_EJ_R_elecownuse_F_Yh")
    L126.out_EJ_R_elecownuse_F_Yh <- get_data(all_data, "L126.out_EJ_R_elecownuse_F_Yh")
    L132.out_EJ_province_indchp_F <- get_data(all_data, "L132.out_EJ_province_indchp_F")

    # -----------------------------------------------------------------------------
    # 2.perform computations
    L132.GWh_province_F_elec_out <- CPSY_GWh_province_F_elec_out %>%
      filter(year %in% HISTORICAL_YEARS_c) %>%
      subset(select = c(-other, -row)) %>%
      map_province_name(province_names_mappings, "province", TRUE) %>%
      # In Tibet, electricity in 'other' will be split between oil, gas, solar, and geothermal in 2010. For prior years
      # it will be moved into coal.
      mutate(oil = 0,
             natural.gas = 0,
             oil = replace(oil, province == "XZ", filter(CPSY_GWh_province_F_elec_out, province.name == "Tibet" & year == 2010)$other / 4),
             natural.gas = replace(oil, province == "XZ", filter(CPSY_GWh_province_F_elec_out, province.name == "Tibet" & year == 2010)$other / 4),
             coal = coal + coal.adj,
             wind = wind + wind.adj,
             nuclear = nuclear + nuc.adj,
             solar = solar + solar.adj) %>%
      subset(select = c(-coal.adj, -wind.adj, -nuc.adj, -solar.adj)) %>%
      mutate(biomass = coal) %>%
      gather(fuel, value, c(-province, -year)) %>%
      filter(value == 0 | value > 0.1) %>%
      complete(nesting(province, fuel), year = HISTORICAL_YEARS_c) %>%
      arrange(province, fuel, year) %>%
      group_by(province, fuel) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup %>%
      mutate(fuel = as.character(fuel)) ->
      L132.GWh_province_F_elec_out

    # Oil and gas does not exist in the CPSY so we will use the CESY for those
    # Note these are in terms of energy in.  Since we are assuming constant efficiencies for all provinces
    # mixing these with the elec_out will be fine for approtioning to provinces
    L101.inNBS_Mtce_province_S_F %>%
      filter(sector == "electricity" & fuel %in% c("refined liquids", "gas")) ->
      L132.Mtce_province_F_elec_in

    L132.Mtce_province_F_elec_in[is.na(L132.Mtce_province_F_elec_in)] <- 0
    L132.Mtce_province_F_elec_in %>%
      subset(select = -sector) %>%
      bind_rows(L132.GWh_province_F_elec_out) ->
      L132.Mix_province_F_elec

    L132.Mix_province_F_elec %>%
      # In Tibet, refined liquids inherits oil and gas inherits natural gas
      mutate(fuel = replace(fuel, fuel == "oil" & province == "XZ", "refined liquids"),
             fuel = replace(fuel, fuel == "natural.gas" & province == "XZ", "gas"),
             # In Tibet our oil and gas numbers are in electricity out and they need to be energy in in Mtce. This is a total hack but
             # just divide by the efficiency. Gas is 0.55 and oil is about 0.35
             tag = 0,
             tag = replace(tag, fuel == "refined liquids" & province == "XZ", 1),
             tag = replace(tag, fuel == "gas" & province == "XZ", 2),
             value = replace(value, tag != 0, 0),
             copy_rl = value,
             copy_g = value,
             copy_rl = copy_rl * 100 * CONV_GWH_EJ * CONV_EJ_MTCE / -0.35,
             copy_g = copy_g * 100 * CONV_GWH_EJ * CONV_EJ_MTCE / -0.55,
             copy_rl = replace(copy_rl, tag != 1 , 0),
             copy_g = replace(copy_g, tag != 2,0),
             value = value + copy_rl +copy_g) %>%
      subset(select = c(-tag, -copy_g, -copy_rl)) ->
      L132.Mix_province_F_elec

    # Aggregate by fuel compute each province's percentage, by fuel
    L132.Mix_province_F_elec %>%
      group_by(fuel, year) %>%
      summarise(sum(value)) %>%
      ungroup ->
      L123.Mix_CHINA_F_elect
    names(L123.Mix_CHINA_F_elect)[3] <- 'Sum'

    L123.pct_province_elec_F <- L132.Mix_province_F_elec %>%
      left_join_error_no_match(L123.Mix_CHINA_F_elect, by = c('year', 'fuel')) %>%
      # All historical solar production was central solar
      mutate(pct = value / Sum,
             fuel = replace(fuel, fuel == 'solar', 'solar PV')) %>%
      subset(select = c(-value, -Sum)) ->
      L123.pct_province_elec_F
    L123.pct_province_elec_F[is.na(L123.pct_province_elec_F)] <- 0

    # Electricity generation inputs by fuel and province
    # Note that fuel inputs are only available for selected fuels; province_share_data uses this relevant subset
    L123.in_EJ_province_elec_F <- L123.in_EJ_R_elec_F_Yh %>%
      filter(GCAM_region_ID == CHINA_REGID & fuel %in% L123.pct_province_elec_F$fuel) ->
      L123.in_EJ_province_elec_F
    Fuel_in_Yh = unique(L123.in_EJ_R_elec_F_Yh$fuel)
    L123.pct_province_elec_F %>%
      filter(fuel %in% Fuel_in_Yh & year %in% HISTORICAL_YEARS) %>%
      left_join_error_no_match(L123.in_EJ_province_elec_F, by = c('year', 'fuel')) %>%
      mutate(elec_F = value * pct) %>%
      subset(select = c(-value, -pct, -GCAM_region_ID)) ->
      L123.in_EJ_province_elec_F

    # Electricity generation outputs by fuel and province
    L123.out_EJ_province_elec_F <- L123.out_EJ_R_elec_F_Yh %>%
      filter(GCAM_region_ID == CHINA_REGID & fuel %in% L123.pct_province_elec_F$fuel) ->
      L123.out_EJ_province_elec_F
    Fuel_out_Yh = unique(L123.in_EJ_R_elec_F_Yh$fuel)
    L123.pct_province_elec_F %>%
      filter(fuel %in% Fuel_out_Yh & year %in% HISTORICAL_YEARS) %>%
      left_join_error_no_match(L123.out_EJ_province_elec_F, by = c('year', 'fuel')) %>%
      mutate(elec_F = value * pct) %>%
      subset(select = c(-value, -pct, -GCAM_region_ID)) ->
      L123.out_EJ_province_elec_F

    # ELECTRICITY - OWNUSE
    # Electricity own use by province
    L123.in_EJ_CHINA_ownuse <- L126.in_EJ_R_elecownuse_F_Yh %>%
      filter(GCAM_region_ID == CHINA_REGID)

    L123.out_EJ_CHINA_ownuse <- L126.out_EJ_R_elecownuse_F_Yh %>%
      filter(GCAM_region_ID == CHINA_REGID)

    L123.net_EJ_CHINA_ownuse <- L123.in_EJ_CHINA_ownuse %>%
      left_join_error_no_match(L123.out_EJ_CHINA_ownuse, by = 'year') %>%
      mutate(value = value.x - value.y,
             fuel = fuel.x,
             sector = sector.x,
             GCAM_region_ID = GCAM_region_ID.x) %>%
      subset(select = c(GCAM_region_ID, sector, fuel, year, value)) ->
      L123.net_EJ_CHINA_ownuse

    # Then build table with each province's share of the national ownuse. Note that this is assumed invariant over time.
    L132.province_elec_out <- L123.out_EJ_province_elec_F %>%
      group_by(province, year) %>%
      summarise(sum(elec_F)) %>%
      ungroup ->
      L132.province_elec_out
    names(L132.province_elec_out)[3] <- 'each'
    L132.province_elec_out %>%
      group_by(year) %>%
      summarise(sum(each)) %>%
      ungroup ->
      L132.province_elec_out_sum
    names(L132.province_elec_out_sum)[2] <- 'Sum'

    L123.net_pct_province_CHINA_ownuse_elec <- L132.province_elec_out %>%
      mutate(sector = 'electricity ownuse',
              fuel = 'electricity') %>%
      left_join_error_no_match(L132.province_elec_out_sum, by = 'year') %>%
      mutate(value = each / Sum) %>%
      subset(select = c(-each, -Sum))->
      L123.net_pct_province_CHINA_ownuse_elec

    # Net own use = national total by each province's share
    L123.net_EJ_province_ownuse_elec <- L123.net_EJ_CHINA_ownuse %>%
      filter(GCAM_region_ID == CHINA_REGID)

    L123.net_pct_province_CHINA_ownuse_elec %>%
      left_join_error_no_match(L123.net_EJ_province_ownuse_elec, by = 'year') %>%
      mutate(value = value.x * value.y,
             fuel = fuel.x,
             sector = sector.x,
             fuel = fuel.x) %>%
      subset(select = c(province, sector, fuel, year, value)) ->
      L123.net_EJ_province_ownuse_elec

    # The input of the electricity_net_ownuse sector is equal to sum of all generation (industrial CHP + electric sector)
    L123.out_EJ_province_elecind_F <- L123.out_EJ_province_elec_F
    names(L123.out_EJ_province_elecind_F)[5] <- 'value'
    L123.out_EJ_province_elecind_F %>%
      bind_rows(L132.out_EJ_province_indchp_F) ->
      L123.out_EJ_province_elecind_F

    L123.in_EJ_province_ownuse_elec <- L123.out_EJ_province_elecind_F %>%
      group_by(province, year) %>%
      summarise(sum(value)) %>%
      ungroup %>%
      mutate(sector = 'electricity ownuse',
             fuel = 'electricity') ->
      L123.in_EJ_province_ownuse_elec
    names(L123.in_EJ_province_ownuse_elec)[3] <- 'value'

    # Output of electricity_net_ownuse sector is equal to input minus ownuse "net" energy
    L123.out_EJ_province_ownuse_elec <- L123.in_EJ_province_ownuse_elec %>%
      left_join_error_no_match(L123.net_EJ_province_ownuse_elec, by = c('province', 'year')) %>%
      mutate(value = value.x - value.y,
             sector = sector.x,
             fuel = fuel.x) %>%
      subset(select = c(province, sector, fuel, year, value)) ->
      L123.out_EJ_province_ownuse_elec

    # ===================================================
    L123.in_EJ_province_elec_F %>%
      add_title("Electricity sector energy consumption by province and fuel") %>%
      add_units("EJ") %>%
      add_comments("The energy consumption is generated by dividing data on capacity factors by province by national average capacity factor") %>%
      add_legacy_name("L123.in_EJ_province_elec_F") %>%
      add_precursors("gcam-china/province_names_mappings",
                     "gcam-china/CPSY_GWh_province_F_elec_out",
                     "L123.in_EJ_R_elec_F_Yh",
                     "L123.out_EJ_R_elec_F_Yh",
                     "L126.in_EJ_R_elecownuse_F_Yh",
                     "L126.out_EJ_R_elecownuse_F_Yh",
                     "L101.inNBS_Mtce_province_S_F",
                     "L132.out_EJ_province_indchp_F") ->
      L123.in_EJ_province_elec_F

    L123.out_EJ_province_elec_F %>%
      add_title("Electricity generation by province and fuel") %>%
      add_units("EJ") %>%
      add_comments("The electricity generation is generated by dividing data on generation by province by fuel") %>%
      add_legacy_name("L123.out_EJ_province_elec_F") %>%
      add_precursors("gcam-china/province_names_mappings",
                     "gcam-china/CPSY_GWh_province_F_elec_out",
                     "L123.in_EJ_R_elec_F_Yh",
                     "L123.out_EJ_R_elec_F_Yh",
                     "L126.in_EJ_R_elecownuse_F_Yh",
                     "L126.out_EJ_R_elecownuse_F_Yh",
                     "L101.inNBS_Mtce_province_S_F",
                     "L132.out_EJ_province_indchp_F") ->
      L123.out_EJ_province_elec_F

    L123.in_EJ_province_ownuse_elec %>%
      add_title("Input to electricity net ownuse by province") %>%
      add_units("EJ") %>%
      add_comments("The electricity net ownuse is generated by dividing data on generation by province by fuel") %>%
      add_legacy_name("L123.in_EJ_province_ownuse_elec") %>%
      add_precursors("gcam-china/province_names_mappings",
                     "gcam-china/CPSY_GWh_province_F_elec_out",
                     "L123.in_EJ_R_elec_F_Yh",
                     "L123.out_EJ_R_elec_F_Yh",
                     "L126.in_EJ_R_elecownuse_F_Yh",
                     "L126.out_EJ_R_elecownuse_F_Yh",
                     "L101.inNBS_Mtce_province_S_F",
                     "L132.out_EJ_province_indchp_F") ->
      L123.in_EJ_province_elec_F

    L123.out_EJ_province_ownuse_elec %>%
      add_title("Output of electricity net ownuse by province") %>%
      add_units("EJ") %>%
      add_comments("The electricity net ownuse is generated by dividing data on generation by province by fuel") %>%
      add_legacy_name("L123.out_EJ_province_ownuse_elec") %>%
      add_precursors("gcam-china/province_names_mappings",
                     "gcam-china/CPSY_GWh_province_F_elec_out",
                     "L123.in_EJ_R_elec_F_Yh",
                     "L123.out_EJ_R_elec_F_Yh",
                     "L126.in_EJ_R_elecownuse_F_Yh",
                     "L126.out_EJ_R_elecownuse_F_Yh",
                     "L101.inNBS_Mtce_province_S_F",
                     "L132.out_EJ_province_indchp_F") ->
      L123.out_EJ_province_ownuse_elec

    return_data(L123.in_EJ_province_elec_F, L123.out_EJ_province_elec_F, L123.in_EJ_province_ownuse_elec, L123.out_EJ_province_ownuse_elec)
  } else {
    stop("Unknown command")
  }
}
