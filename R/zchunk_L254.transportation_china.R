#' module_gcam.china_L254.transportation_china
#'
#' Generates GCAM-USA model inputs for transportation sector by states.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs:
#' original data system was \code{L254.transportation_CHINA.R} (gcam-china level2).
#' @details This chunk generates input files for transportation sector with generic information for supplysector,
#' subsector and technologies, as well as calibrated inputs and outputs by the China provinces.
#' @note The transportation structure is heavily nested. The GCAM structure of sector/subsector/technology only
#' allows two levels of nesting within any sector, but a technology of one sector (e.g., trn_pass) can consume the
#' output of another "sector" (e.g., trn_pass_road) that is really just used to represent lower nesting levels of
#' that first, or parent, sector. In the transportation sector, each lower-level nesting "sector" is named by
#' appending a string to the parent sector. So, \code{trn_pass} contains \code{trn_pass_road} which has
#' \code{trn_pass_road_LDV} which has \code{trn_pass_road_LDV_4W}. Each of the links between any two of those sectors
#' is done with a pass-through technology within the parent sector that consumes the output of the child sector.
#' The technology is called a "pass-through" because it (generally) only consumes the output of the child "sector"
#' without making any changes to it. There's an additional complication in the transportation sector, that the
#' pass-through technologies are normal, standard GCAM technologies, not "tranTechnologies" which have different
#' parameters read in, and perform a bunch of hard-wired unit conversions between inputs and outputs.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter if_else group_by left_join mutate select semi_join summarise
#' @importFrom tidyr gather spread
#' @author BY Jul 2019

module_gcam.china_L254.transportation_china <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/province_names_mappings",
             "L254.Supplysector_trn",
             "L254.FinalEnergyKeyword_trn",
             "L254.tranSubsectorLogit",
             "L254.tranSubsectorShrwtFllt",
             "L254.tranSubsectorInterp",
             "L254.tranSubsectorSpeed",
             "L254.tranSubsectorSpeed_passthru",
             "L254.tranSubsectorSpeed_noVOTT",
             "L254.tranSubsectorSpeed_nonmotor",
             "L254.tranSubsectorVOTT",
             "L254.tranSubsectorFuelPref",
             "L254.StubTranTech",
             "L254.StubTech_passthru",
             "L254.StubTech_nonmotor",
             "L254.StubTranTechLoadFactor",
             "L254.StubTranTechCost",
             "L254.StubTranTechCoef",
             "L254.PerCapitaBased_trn",
             "L254.PriceElasticity_trn",
             "L254.IncomeElasticity_trn"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c())
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]


    # Load required inputs
    province_names_mappings <- get_data(all_data, "gcam-china/province_names_mappings")
    L254.Supplysector_trn <- get_data(all_data, "L254.Supplysector_trn")
    L254.FinalEnergyKeyword_trn <- get_data(all_data, "L254.FinalEnergyKeyword_trn")
    L254.tranSubsectorLogit <- get_data(all_data, "L254.tranSubsectorLogit")
    L254.tranSubsectorShrwtFllt <- get_data(all_data, "L254.tranSubsectorShrwtFllt")
    L254.tranSubsectorInterp <- get_data(all_data, "L254.tranSubsectorInterp")
    L254.tranSubsectorSpeed <- get_data(all_data, "L254.tranSubsectorSpeed")
    L254.tranSubsectorSpeed_passthru <- get_data(all_data, "L254.tranSubsectorSpeed_passthru")
    L254.tranSubsectorSpeed_noVOTT <- get_data(all_data, "L254.tranSubsectorSpeed_noVOTT")
    L254.tranSubsectorSpeed_nonmotor <- get_data(all_data, "L254.tranSubsectorSpeed_nonmotor")
    L254.tranSubsectorVOTT <- get_data(all_data, "L254.tranSubsectorVOTT")
    L254.tranSubsectorFuelPref <- get_data(all_data, "L254.tranSubsectorFuelPref")
    L254.StubTranTech <- get_data(all_data, "L254.StubTranTech")
    L254.StubTech_passthru <- get_data(all_data, "L254.StubTech_passthru")
    L254.StubTech_nonmotor <- get_data(all_data, "L254.StubTech_nonmotor")
    L254.StubTranTechLoadFactor <- get_data(all_data, "L254.StubTranTechLoadFactor")
    L254.StubTranTechCost <- get_data(all_data, "L254.StubTranTechCost")
    L254.StubTranTechCoef <- get_data(all_data, "L254.StubTranTechCoef")
    L254.PerCapitaBased_trn <- get_data(all_data, "L254.PerCapitaBased_trn")
    L254.PriceElasticity_trn <- get_data(all_data, "L254.PriceElasticity_trn")
    L254.IncomeElasticity_trn <- get_data(all_data, "L254.IncomeElasticity_trn")

    # Need to delete the transportation sector in the CHINA region (energy-final-demands and supplysectors)
    # L254.DeleteSupplysector_CHINAtrn: Delete transportation supplysectors of the USA region
    L254.Supplysector_trn %>%
      mutate(region = region) %>% # strip off attributes like title, etc.
      filter(region == gcamchina.REGION) %>%
      select(region, supplysector) ->
      L254.DeleteSupplysector_CHINAtrn

    # L254.DeleteFinalDemand_CHINAtrn: Delete energy final demand sectors of the CHINA region
    L254.PerCapitaBased_trn %>%
      mutate(region = region) %>% # strip off attributes like title, etc.
      filter(region == gcam.USA_REGION) %>%
      select(LEVEL2_DATA_NAMES[["EnergyFinalDemand"]]) ->
      L254.DeleteFinalDemand_CHINAtrn

    # Process tables at the CHINA region level to the province level.
    # All tables for which processing is identical are done by a function.
    # This applies to the supplysectors, subsectors, and stub tech characteristics of the states.
    process_CHINA_to_provinces <- function(data) {
      state <- region <- grid_region <- subsector <- market.name <-
        minicam.energy.input <- NULL  # silence package check notes

      data_new <- data %>%
        filter(region == gcamchina.REGION) %>%
        write_to_all_trn_provinces(names = c(names(data), "region"))

      # Re-set markets from CHINA to regional markets, if called for in the GCAM-China assumptions for selected fuels
      if(gcamchina.USE_REGIONAL_FUEL_MARKETS & "market.name" %in% names(data_new)) {
        data_new <- data_new %>%
          left_join_error_no_match(select(province_names_mappings, province, grid.region), by = c("region" = "province")) %>%
          mutate(market.name = replace(market.name, minicam.energy.input %in% gcamchina.REGIONAL_FUEL_MARKETS,
                                       grid.region[minicam.energy.input %in% gcamchina.REGIONAL_FUEL_MARKETS])) %>%
          select(-grid.region)
      }

      # Electricity is always consumed from province markets
      if("market.name" %in% names(data_new)) {
        data_new <- data_new %>%
          mutate(market.name = replace(market.name, minicam.energy.input %in% gcamchina.ELECT_TD_SECTORS,
                                       region[minicam.energy.input %in% gcamchina.ELECT_TD_SECTORS]))
      }

      data_new
    }

    process_CHINA_to_provinces(L254.Supplysector_trn) -> L254.Supplysector_trn_CHINA #has extra column logit.type (with mostly NA, absolute-cost-logit for trn_pass_road_LDV_4W)
    process_CHINA_to_provinces(L254.FinalEnergyKeyword_trn) -> L254.FinalEnergyKeyword_trn_CHINA
    process_CHINA_to_provinces(L254.tranSubsectorLogit) -> L254.tranSubsectorLogit_CHINA #has extra column logit.type with absolute-cost-logit
    process_CHINA_to_provinces(L254.tranSubsectorShrwtFllt) -> L254.tranSubsectorShrwtFllt_CHINA
    process_CHINA_to_provinces(L254.tranSubsectorInterp) -> L254.tranSubsectorInterp_CHINA
    process_CHINA_to_provinces(L254.tranSubsectorSpeed) -> L254.tranSubsectorSpeed_CHINA
    process_CHINA_to_provinces(L254.tranSubsectorSpeed_passthru) -> L254.tranSubsectorSpeed_passthru_CHINA
    process_CHINA_to_provinces(L254.tranSubsectorSpeed_noVOTT) -> L254.tranSubsectorSpeed_noVOTT_CHINA
    process_CHINA_to_provinces(L254.tranSubsectorSpeed_nonmotor) -> L254.tranSubsectorSpeed_nonmotor_CHINA
    process_CHINA_to_provinces(L254.tranSubsectorVOTT) -> L254.tranSubsectorVOTT_CHINA
    process_CHINA_to_provinces(L254.tranSubsectorFuelPref) -> L254.tranSubsectorFuelPref_CHINA
    process_CHINA_to_provinces(L254.StubTranTech) -> L254.StubTranTech_CHINA
    process_CHINA_to_provinces(L254.StubTech_passthru) -> L254.StubTranTech_passthru_CHINA
    process_CHINA_to_provinces(L254.StubTech_nonmotor) -> L254.StubTranTech_nonmotor_CHINA
    process_CHINA_to_provinces(L254.StubTranTechLoadFactor) -> L254.StubTranTechLoadFactor_CHINA
    process_CHINA_to_provinces(L254.StubTranTechCost) -> L254.StubTranTechCost_CHINA

    L254.StubTranTechCoef %>%
      mutate(coefficient = round(coefficient, digits = gcamchina.DIGITS_TRNCHINA_DEFAULT)) %>%
      process_CHINA_to_provinces ->
      L254.StubTranTechCoef_CHINA

    process_CHINA_to_provinces(L254.PerCapitaBased_trn) -> L254.PerCapitaBased_trn_CHINA
    process_CHINA_to_provinces(L254.PriceElasticity_trn) -> L254.PriceElasticity_trn_CHINA
    process_CHINA_to_provinces(L254.IncomeElasticity_trn) -> L254.IncomeElasticity_trn_CHINA

  #Calibration
  } else {
    stop("Unknown command")
  }
}
