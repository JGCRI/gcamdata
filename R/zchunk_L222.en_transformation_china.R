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
             FILE="energy/calibrated_techs",
             "L222.StubTechCoef_refining",
             "L222.GlobalTechInterp_en",
             "L222.GlobalTechCoef_en",
             "L222.GlobalTechCost_en",
             "L222.GlobalTechShrwt_en",
             "L222.GlobalTechCapture_en",
             # "L222.GlobalTechShutdownProfit_en",
             # "L222.GlobalTechShutdown_en",
             # "L222.GlobalTechSCurveProfit_en",
             "L222.GlobalTechSCurve_en",
             # "L222.GlobalTechLifetimeProfit_en",
             # "L222.GlobalTechLifetime_en",
             "L222.Supplysector_en",
             "L202.CarbonCoef"
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
    L222.StubTechCoef_refining <- get_data(all_data, "L222.StubTechCoef_refining")
    L222.GlobalTechInterp_en <- get_data(all_data, "L222.GlobalTechInterp_en")
    L222.GlobalTechCoef_en <- get_data(all_data, "L222.GlobalTechCoef_en")
    L222.GlobalTechCost_en <- get_data(all_data, "L222.GlobalTechCost_en")
    L222.GlobalTechShrwt_en <- get_data(all_data, "L222.GlobalTechShrwt_en")
    L222.GlobalTechCapture_en <- get_data(all_data, "L222.GlobalTechCapture_en")
    # L222.GlobalTechShutdownProfit_en <- get_data(all_data, "L222.GlobalTechShutdownProfit_en")
    # L222.GlobalTechShutdown_en <- get_data(all_data, "L222.GlobalTechShutdown_en")
    # L222.GlobalTechSCurveProfit_en <- get_data(all_data, "L222.GlobalTechSCurveProfit_en")
    L222.GlobalTechSCurve_en <- get_data(all_data, "L222.GlobalTechSCurve_en")
    # L222.GlobalTechLifetimeProfit_en <- get_data(all_data, "L222.GlobalTechLifetimeProfit_en")
    # L222.GlobalTechLifetime_en <- get_data(all_data, "L222.GlobalTechLifetime_en")
    L222.Supplysector_en <- get_data(all_data, "L222.Supplysector_en")
    L202.CarbonCoef <- getdata(all_data, "L202.CarbonCoef")

    # ===================================================
    # Define sector(s) used in L222.en_transformation_China
    # The supplysector and subsector structure in these sectors are retained
    gcamchina.SECTOR_EN_NAMES <- "refining"
    gcamchina.SECTOR_oil_refining <- "oil refining"
    gcamchina.SECTOR_bio_liquids <- "biomass liquids"

    # NOTE: Oil refining sectors are only created in provinces where the production is >0 in the historical period. Other techs are available everywhere
    L122.out_EJ_province_refining_F %>%
      filter(sector == gcamchina.SECTOR_oil_refining) %>%
      group_by(province) %>%
      summarize(value = sum(value)) %>%
      ungroup() %>%
      filter(value > 0) %>%
      select(province) ->
      oil_refining_provinces

    # ===================================================
    # Some helpful functions:
    #
    # global_energy_to_China_nonGlobalTech - takes global energy inputs for non global tech
    # from L222.en_transformation.R and processes for use in China
    global_energy_to_China_nonGlobalTech <- function(data) {
      data %>%
        filter(region == gcamchina.REGION,
               supplysector %in% gcamchina.SECTOR_EN_NAMES) %>%
        write_to_all_provinces(names = c(names(data), "region")) %>%
        filter((subsector == "oil refining" & region %in% oil_refining_provinces$province) |
                 subsector != "oil refining") %>%
        mutate(supplysector = subsector)
    } # global_energy_to_China_nonGlobalTech

    # global_energy_to_China_GlobalTech - takes global energy inputs for global tech
    # from L222.en_transformation.R and processes for use in USA
    global_energy_to_China_GlobalTech <- function(data) {
      data %>%
        filter(sector.name %in% gcamchina.SECTOR_EN_NAMES) %>%
        mutate(sector.name = subsector.name)
    } # global_energy_to_China_GlobalTech

    # ===================================================

    # L222.DeleteStubTech_CHINAen: remove existing stub technologies in the CHINA region
    # The supplysector and subsector structure in the CHINA refining sector is retained
    L222.StubTech_en %>%
      filter(region == gcamchina.REGION,
             supplysector %in% gcamchina.SECTOR_EN_NAMES) ->
      L222.DeleteStubTech_CHINAen

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
      filter(year %in% MODEL_BASE_YEARS) %>%
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

    # Process energy files from L222.en_transformation.R for use in China,
    # slightly differently processing for global tech vs not inputs
    L222.SubsectorLogit_en_CHINA      <- global_energy_to_China_nonGlobalTech(L222.SubsectorLogit_en) #has an extra column logit.type : absolute-cost-logit
    L222.StubTech_en_CHINA            <- global_energy_to_China_nonGlobalTech(L222.StubTech_en) #doesn't have data for oil refining in GZ, SX, XZ, and all data in HK, MC
    L222.StubTechCoef_refining_CHINA  <- global_energy_to_China_nonGlobalTech(L222.StubTechCoef_refining) #doesn't have oil refining data for GZ, SX, XZ, HK, MC
    L222.GlobalTechInterp_en_CHINA    <- global_energy_to_China_GlobalTech(L222.GlobalTechInterp_en)
    L222.GlobalTechCoef_en_CHINA      <- global_energy_to_China_GlobalTech(L222.GlobalTechCoef_en)
    L222.GlobalTechCost_en_CHINA      <- global_energy_to_China_GlobalTech(L222.GlobalTechCost_en)
    L222.GlobalTechShrwt_en_CHINA     <- global_energy_to_China_GlobalTech(L222.GlobalTechShrwt_en)
    L222.GlobalTechCapture_en_CHINA   <- global_energy_to_China_GlobalTech(L222.GlobalTechCapture_en)
    L222.GlobalTechSCurve_en_CHINA    <- global_energy_to_China_GlobalTech(L222.GlobalTechSCurve_en)

    ### The same processing for optional/currently NULL inputs
    # L222.GlobalTechShutdownProfit_en_CHINA  <- global_energy_to_China_GlobalTech(L222.GlobalTechShutdownProfit_en)
    # L222.GlobalTechShutdown_en_CHINA        <- global_energy_to_China_GlobalTech(L222.GlobalTechShutdown_en)
    # L222.GlobalTechSCurveProfit_en_CHINA    <- global_energy_to_China_GlobalTech(L222.GlobalTechSCurveProfit_en)
    # L222.GlobalTechLifetimeProfit_en_CHINA  <- global_energy_to_China_GlobalTech(L222.GlobalTechLifetimeProfit_en)
    # L222.GlobalTechLifetime_en_CHINA        <- global_energy_to_China_GlobalTech(L222.GlobalTechLifetime_en)

    # L222.Supplysector_en_CHINA: Supplysector information, replace name of supplysector with the subsector names
    L222.SubsectorLogit_en_CHINA %>%
      select(one_of(LEVEL2_DATA_NAMES[["Subsector"]])) %>%
      left_join_error_no_match(distinct(select(L222.SubsectorLogit_en, supplysector, subsector)),
                               by = "subsector") %>%
      rename(supplysector = supplysector.x,
             old_supplysector = supplysector.y) %>%
      left_join_error_no_match(distinct(select(L222.Supplysector_en, -region)),
                               by = c("old_supplysector" = "supplysector")) %>%
      select(one_of(LEVEL2_DATA_NAMES[["Supplysector"]])) ->
      L222.Supplysector_en_CHINA

    # L222.Supplysector_en_CHINA_logit.type - Note there is no competition here so just use the default logit type
    L222.Supplysector_en_CHINA %>%
      mutate(logit.type = gcamchina.DEFAULT_LOGIT_TYPE) ->
      L222.Supplysector_en_CHINA_logit.type

    # L222.SubsectorShrwtFllt_en_CHINA: Subsector shareweights, there is no competition here, so just fill out with 1s
    # (will be over-ridden by base year calibration where necessary)
    L222.SubsectorLogit_en_CHINA %>%
      select(one_of(LEVEL2_DATA_NAMES[["Subsector"]])) %>%
      mutate(year = min(MODEL_YEARS),
             share.weight = gcamchina.DEFAULT_SHAREWEIGHT) ->
      L222.SubsectorShrwtFllt_en_CHINA

    # L222.StubTechProd_refining_CHINA: calibrated fuel production by province
    #
    # Step 1, process the table of calibrated_techs to only include calibration=output and relevant columns
    calibrated_techs %>%
      filter(calibration == "output") %>%
      select(sector, supplysector, subsector, technology) %>%
      distinct ->
      calibrated_techs_tmp

    # Step 2, process L122.out_EJ_province_refining_F, joining the processed table of calibrated_techs from step 1,
    # to create L222.StubTechProd_refining_CHINA. Note the supplysector is the same as the subsector within the states.
    L122.out_EJ_province_refining_F %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      rename(region = province,
             calOutputValue = value) %>%
      mutate(calOutputValue = round(calOutputValue, gcamchina.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(calibrated_techs_tmp, by = "sector") %>%
      #The supplysector is the same as the subsector within the provinces
      mutate(supplysector = subsector,
             stub.technology = technology,
             share.weight.year = year) %>%
      set_subsector_shrwt() %>%
      mutate(tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(one_of(LEVEL2_DATA_NAMES[["StubTechProd"]])) %>%
      # Remove oil refining tech from provinces that do not have any
      filter((subsector == "oil refining" & region %in% oil_refining_provinces$province) |
               subsector != "oil refining") ->
      L222.StubTechProd_refining_CHINA

    # L222.StubTechMarket_en_CHINA: market names of inputs to province refining sectors
    L222.GlobalTechCoef_en_CHINA %>%
      select(one_of(LEVEL2_DATA_NAMES[["GlobalTechInput"]])) %>%
      write_to_all_provinces(names = c(LEVEL2_DATA_NAMES[["GlobalTechInput"]], "region")) %>%
      rename(supplysector = sector.name,
             subsector = subsector.name,
             stub.technology = technology) %>%
      mutate(market.name = gcamchina.REGION) ->
      L222.StubTechMarket_en_CHINA

    # Finish L222.StubTechMarket_en_CHINA by Setting electricity to the province markets
    L222.StubTechMarket_en_CHINA %>%
      filter(minicam.energy.input %in% gcamchina.ELECT_TD_SECTORS) %>%
      mutate(market.name = region) ->
      tmp

    # create a key for filtering
    L222.StubTech_en_CHINA %>%
      select(supplysector, subsector, stub.technology) %>%
      unite(key, supplysector, subsector, stub.technology, sep = "~") %>%
      distinct ->
      L222.StubTech_en_CHINA_key

    L222.StubTechMarket_en_CHINA %>%
      filter(!(minicam.energy.input %in% gcamchina.ELECT_TD_SECTORS)) %>%
      bind_rows(tmp) %>%
      select(one_of(LEVEL2_DATA_NAMES[["StubTechMarket"]])) %>%
      unite(key, supplysector, subsector, stub.technology, sep = "~") %>%
      filter(key %in% L222.StubTech_en_CHINA_key$key) %>%
      separate(key, c("supplysector", "subsector", "stub.technology"), sep = "~") %>%
      filter((subsector == "oil refining" & region %in% oil_refining_provinces$province) |
               subsector != "oil refining") ->
      L222.StubTechMarket_en_CHINA

    # L222.CarbonCoef_en_CHINA: energy carbon coefficients in China
    #
    # Step 1, process L202.CarbonCoef for joining
    L202.CarbonCoef %>%
      filter(region == gcamchina.REGION) %>%
      select(-region) %>%
      distinct ->
      L202.CarbonCoef_tmp

    # Step 2, create L222.CarbonCoef_en_CHINA by joining the table from step 1.
    L222.Supplysector_en_CHINA %>%
      select(region, supplysector) %>%
      distinct %>%
      left_join_error_no_match(distinct(select(L222.TechShrwt_CHINAen, subsector, supplysector)),
                               by = c("supplysector" = "subsector")) %>%
      left_join_error_no_match(L202.CarbonCoef_tmp, by =  c("supplysector.y" = "PrimaryFuelCO2Coef.name")) %>%
      select(-supplysector.y) %>%
      rename(PrimaryFuelCO2Coef.name = supplysector) ->
      L222.CarbonCoef_en_CHINA


  } else {
    stop("Unknown command")
  }
}
