#' module_gcam.china_L222.en_transformation_china
#'
#' Prepare the assumptions and calibrated outputs for energy transformation supplysectors, subsectors, and technologies specific to China sectors and/or provinces.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L222.DeleteStubTech_USAen}, \code{L222.PassThroughSector_USAen}, \code{L222.Tech_USAen}, \code{L222.TechShrwt_USAen}, \code{L222.TechInterp_USAen}, \code{L222.TechShrwt_USAen}, \code{L222.TechCoef_USAen}, \code{L222.Production_USArefining}, \code{L222.SectorLogitTables_USA[[ curr_table ]]$data}, \code{L222.Supplysector_en_USA}, \code{L222.SubsectorShrwtFllt_en_USA}, \code{L222.StubTechProd_refining_USA}, \code{L222.StubTechMarket_en_USA}, \code{L222.CarbonCoef_en_USA}. The corresponding file in the
#' original data system was \code{L222.en_transformation_USA.R} (gcam-usa level2).
#' @details This chunk sets up the USA energy transformation technology databases as well as writing out assumptions to all states/sectors/markets for shareweights and logits.
#' Calibrated outputs and I:O coefficients are updated from global values produced by \code{\link{module_energy_L222.en_transformation}}.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else group_by left_join mutate one_of pull select summarise
#' @importFrom tidyr gather spread
#' @author BY Jun 2019

module_gcam.china_L222.en_transformation_china <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L222.StubTech_en",
             "L122.out_EJ_province_refining_F",
             "L222.SubsectorLogit_en",
            FILE="energy/calibrated_techs"
             ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c())
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L222.StubTech_en <- get_data(all_data, "L222.StubTech_en")
    L122.out_EJ_province_refining_F <- get_data(all_data, "L122.out_EJ_province_refining_F")
    L222.SubsectorLogit_en <- get_data(all_data, "L222.SubsectorLogit_en")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")

    # ===================================================
    # Define sector(s) used in L222.en_transformation_China
    # The supplysector and subsector structure in these sectors are retained
    gcamchina.SECTOR_EN_NAMES <- "refining"
    gcamchina.SECTOR_oil_refining <- "oil refining"
    gcamchina.SECTOR_bio_liquids <- "biomass liquids"

    # L222.DeleteStubTech_CHINAen: remove existing stub technologies in the CHINA region
    # The supplysector and subsector structure in the CHINA refining sector is retained
    L222.StubTech_en %>%
      filter(region == gcamchina.REGION,
             supplysector %in% gcamchina.SECTOR_EN_NAMES) ->
      L222.DeleteStubTech_CHINAen

    # NOTE: Oil refining sectors are only created in provinces where the production is >0 in the historical period. Other techs are available everywhere
    L122.out_EJ_province_refining_F %>%
      filter(sector == gcamchina.SECTOR_oil_refining) %>%
      group_by(province) %>%
      summarize(value = sum(value)) %>%
      ungroup() %>%
      filter(value > 0) %>%
      select(province) ->
      oil_refining_provinces

    # L222.TechEQUIV
    L222.TechEQUIV <- tibble(group.name = "technology",
                             tag1 = "technology",
                             tag2 = "pass-through-technology")

    # L222.Tech_CHINAen
    L222.SubsectorLogit_en %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      filter(region == gcamchina.REGION, supplysector %in% gcamchina.SECTOR_EN_NAMES) %>%
      repeat_add_columns(tibble::tibble(province = gcamchina.PROVINCES) ) %>%
      filter(subsector == gcamchina.SECTOR_oil_refining & province %in% oil_refining_provinces$province
             | subsector != gcamchina.SECTOR_oil_refining) %>%
      mutate(technology = paste(province, subsector, sep = " ")) ->
      L222.Tech_CHINAen

    # L222.PassThroughSector_CHINAen: PassThroughSector information to send vintaging info from provinces to CHINA
    L222.Tech_CHINAen %>%
      select(province, LEVEL2_DATA_NAMES[["Subsector"]]) ->
      L222.PassThroughSector_CHINAen
    names(L222.PassThroughSector_CHINAen) <- LEVEL2_DATA_NAMES[["PassThroughSector"]]

    L222.Tech_CHINAen %>%
      select(LEVEL2_DATA_NAMES[["Tech"]]) ->
      L222.Tech_CHINAen

    # L222.TechInterp_CHINAen: technology shareweights, CHINA region
    # NOTE: Technology interpolation only applies to calibrated technologies
    L222.Tech_CHINAen %>%
      filter(subsector %in% c(gcamchina.SECTOR_oil_refining, gcamchina.SECTOR_bio_liquids)) %>%
      mutate(apply.to = "share-weight",
             from.year = max(MODEL_BASE_YEARS),
             to.year = max(MODEL_FUTURE_YEARS),
            # For biomass liquids, allow province shares to shift over time (future techs are different than present techs)
            interpolation.function = if_else(subsector == gcamchina.SECTOR_bio_liquids, "s-curve", "fixed")) ->
      L222.TechInterp_CHINAen

    # L222.TechShrwt_CHINAen: technology shareweights, CHINA region
    L222.Tech_CHINAen %>%
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) %>%
      # Default the base year shareweights to 0. This will be over-ridden in calibration
      mutate(share.weight = case_when(year %in% MODEL_BASE_YEARS ~ 0,
                                      year %in% MODEL_FUTURE_YEARS ~ 1)) ->
      L222.TechShrwt_CHINAen


    # L222.TechCoef_CHINAen: technology coefficients and market names, CHINA region
    L222.TechShrwt_CHINAen %>%
      select(LEVEL2_DATA_NAMES[["Tech"]]) %>%
      mutate(minicam.energy.input = subsector,
             coefficient = 1,
             # market.name is the province name, extracting it from the technology
             market.name = substr(technology, 1, nchar(technology) - nchar(subsector) - 1)) ->
      L222.TechCoef_CHINAen

    # L222.Production_CHINArefining: calibrated refinery production in CHINA (consuming output of provinces)
    L122.out_EJ_province_refining_F %>%
      mutate(value = round(value, digits = gcamchina.DIGITS_CALOUTPUT),
             region = gcamchina.REGION) %>%
      rename(calOutputValue = value) %>%
      left_join_keep_first_only(select(calibrated_techs, sector, supplysector, subsector), by = "sector") %>%
      mutate(technology = paste(province, subsector, sep = " "),
             minicam.energy.input = subsector) %>%
      filter(subsector == gcamchina.SECTOR_oil_refining & province %in% oil_refining_provinces$province
             | subsector != gcamchina.SECTOR_oil_refining) %>%
      group_by(region, supplysector, subsector, technology, year, minicam.energy.input) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>%
      ungroup() %>%
      set_subsector_shrwt() %>%
      mutate(share.weight.year = year,
             tech.share.weight = if_else(calOutputValue == 0, 0, 1)) %>%
      select(LEVEL2_DATA_NAMES[["Production"]]) ->
      L222.Production_CHINArefining

    #L222.SectorEQUIV
    L222.SectorEQUIV <- tibble(group.name = "sector",
                               tag1 = "supplysector",
                               tag2 = "pass-through-sector")

    # Process energy files from L222.en_transformation.R for use in the USA,
    # slightly differently processing for global tech vs not inputs
    L222.SubsectorLogit_en_USA      <- global_energy_to_USA_nonGlobalTech(L222.SubsectorLogit_en)
    L222.StubTech_en_USA            <- global_energy_to_USA_nonGlobalTech(L222.StubTech_en)
    L222.StubTechCoef_refining_USA  <- global_energy_to_USA_nonGlobalTech(L222.StubTechCoef_refining)
    L222.GlobalTechInterp_en_USA    <- global_energy_to_USA_GlobalTech(L222.GlobalTechInterp_en)
    L222.GlobalTechCoef_en_USA      <- global_energy_to_USA_GlobalTech(L222.GlobalTechCoef_en)
    L222.GlobalTechCost_en_USA      <- global_energy_to_USA_GlobalTech(L222.GlobalTechCost_en)
    L222.GlobalTechShrwt_en_USA     <- global_energy_to_USA_GlobalTech(L222.GlobalTechShrwt_en)
    L222.GlobalTechCapture_en_USA   <- global_energy_to_USA_GlobalTech(L222.GlobalTechCapture_en)
    L222.GlobalTechSCurve_en_USA    <- global_energy_to_USA_GlobalTech(L222.GlobalTechSCurve_en)


    L222.tables <- list( L222.SubsectorLogit_en = L222.SubsectorLogit_en,
                         L222.StubTech_en = L222.StubTech_en,
                         L222.StubTechCoef_refining = L222.StubTechCoef_refining,
                         L222.GlobalTechInterp_en = L222.GlobalTechInterp_en,
                         L222.GlobalTechCoef_en = L222.GlobalTechCoef_en,
                         L222.GlobalTechCost_en = L222.GlobalTechCost_en,
                         L222.GlobalTechShrwt_en = L222.GlobalTechShrwt_en,
                         L222.GlobalTechCapture_en = L222.GlobalTechCapture_en,
                         L222.GlobalTechShutdownProfit_en = L222.GlobalTechShutdownProfit_en,
                         L222.GlobalTechShutdown_en = L222.GlobalTechShutdown_en,
                         L222.GlobalTechSCurveProfit_en = L222.GlobalTechSCurveProfit_en,
                         L222.GlobalTechSCurve_en = L222.GlobalTechSCurve_en,
                         L222.GlobalTechLifetimeProfit_en = L222.GlobalTechLifetimeProfit_en,
                         L222.GlobalTechLifetime_en = L222.GlobalTechLifetime_en

  } else {
    stop("Unknown command")
  }
}
