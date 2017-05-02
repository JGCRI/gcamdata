#' module_aglu_LB125.LC_tot
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L125.LC_bm2_R}, \code{L125.LC_bm2_R_GLU}, \code{L125.LC_bm2_R_LT_Yh_GLU}. The corresponding file in the
#' original data system was \code{LB125.LC_tot.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
module_aglu_LB125.LC_tot <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( "L120.LC_bm2_R_UrbanLand_Yh_GLU",
              "L120.LC_bm2_R_Tundra_Yh_GLU",
              "L120.LC_bm2_R_RckIceDsrt_Yh_GLU",
              FILE = "temp-data-inject/L122.LC_bm2_R_HarvCropLand_Yh_GLU",
              FILE = "temp-data-inject/L122.LC_bm2_R_OtherArableLand_Yh_GLU",
              FILE = "temp-data-inject/L123.LC_bm2_R_MgdPast_Yh_GLU",
              FILE = "temp-data-inject/L123.LC_bm2_R_MgdFor_Yh_GLU",
              FILE = "temp-data-inject/L124.LC_bm2_R_Shrub_Yh_GLU_adj",
              FILE = "temp-data-inject/L124.LC_bm2_R_Grass_Yh_GLU_adj",
              FILE = "temp-data-inject/L124.LC_bm2_R_UnMgdPast_Yh_GLU_adj",
              FILE = "temp-data-inject/L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L125.LC_bm2_R",
             "L125.LC_bm2_R_GLU",
             "L125.LC_bm2_R_LT_Yh_GLU"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L120.LC_bm2_R_UrbanLand_Yh_GLU <- get_data(all_data, "L120.LC_bm2_R_UrbanLand_Yh_GLU")
    L120.LC_bm2_R_Tundra_Yh_GLU <- get_data(all_data, "L120.LC_bm2_R_Tundra_Yh_GLU")
    L120.LC_bm2_R_RckIceDsrt_Yh_GLU <- get_data(all_data, "L120.LC_bm2_R_RckIceDsrt_Yh_GLU")
    L122.LC_bm2_R_HarvCropLand_Yh_GLU <- get_data(all_data, "temp-data-inject/L122.LC_bm2_R_HarvCropLand_Yh_GLU")
    L122.LC_bm2_R_OtherArableLand_Yh_GLU <- get_data(all_data, "temp-data-inject/L122.LC_bm2_R_OtherArableLand_Yh_GLU")
    L123.LC_bm2_R_MgdPast_Yh_GLU <- get_data(all_data, "temp-data-inject/L123.LC_bm2_R_MgdPast_Yh_GLU")
    L123.LC_bm2_R_MgdFor_Yh_GLU <- get_data(all_data, "temp-data-inject/L123.LC_bm2_R_MgdFor_Yh_GLU")
    L124.LC_bm2_R_Shrub_Yh_GLU_adj <- get_data(all_data, "temp-data-inject/L124.LC_bm2_R_Shrub_Yh_GLU_adj")
    L124.LC_bm2_R_Grass_Yh_GLU_adj <- get_data(all_data, "temp-data-inject/L124.LC_bm2_R_Grass_Yh_GLU_adj")
    L124.LC_bm2_R_UnMgdPast_Yh_GLU_adj <- get_data(all_data, "temp-data-inject/L124.LC_bm2_R_UnMgdPast_Yh_GLU_adj")
    L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj <- get_data(all_data, "temp-data-inject/L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj")

    # ===================================================
    # TRANSLATED PROCESSING CODE GOES HERE...
    #
    # If you find a mistake/thing to update in the old code and
    # fixing it will change the output data, causing the tests to fail,
    # (i) open an issue on GitHub, (ii) consult with colleagues, and
    # then (iii) code a fix:
    #
    # if(OLD_DATA_SYSTEM_BEHAVIOR) {
    #   ... code that replicates old, incorrect behavior
    # } else {
    #   ... new code with a fix
    # }
    #
    #
    # ===================================================

    # -----------------------------------------------------------------------------
    # 2. Perform computations
    #Create a table with all land types
    #NOTE: If protected lands are included, uncomment the text below
    bind_rows(L122.LC_bm2_R_HarvCropLand_Yh_GLU,
              L122.LC_bm2_R_OtherArableLand_Yh_GLU,
              L123.LC_bm2_R_MgdPast_Yh_GLU,
              L123.LC_bm2_R_MgdFor_Yh_GLU,
              L124.LC_bm2_R_Shrub_Yh_GLU_adj,
              L124.LC_bm2_R_Grass_Yh_GLU_adj,
              L124.LC_bm2_R_UnMgdPast_Yh_GLU_adj,
              L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj) %>%
      # The next two lines are TEMPORARY - because currently we're using temp-data-inject data
      # When using final data, don't need to reshape them
      gather(year, value, -GCAM_region_ID, -Land_Type, -GLU) %>%
      mutate(year = as.integer(substr(year, 2, 5))) %>%
      bind_rows(L120.LC_bm2_R_UrbanLand_Yh_GLU,
                L120.LC_bm2_R_Tundra_Yh_GLU,
                L120.LC_bm2_R_RckIceDsrt_Yh_GLU)  ->
      L125.LC_bm2_R_LT_Yh_GLU

    L125.LC_bm2_R_LT_Yh_GLU %>%
      group_by(GCAM_region_ID, GLU, year) %>%               # group the data by GCAM_reigon_id and GLU
      summarise(value = sum(value)) ->                      # Adding up total land area by region, year, and GLU
      L125.LC_bm2_R_Yh_GLU

    # prepare the temporary varible for calculating the changing rate
    L125.LC_bm2_R_Yh_GLU %>%
      spread(key="year",value=value) %>%                   # convert to wide format
      ungroup() %>%
      select(-GCAM_region_ID,-GLU) ->                      # remove the unnecessary columns
      LC_check1 ->                                         # temporary variable 1
      LC_check2                                            # temporary variable 2

    LC_check2[,2:ncol(LC_check2)] <- LC_check1[,1:ncol(LC_check2)-1]   # shift the columns of temporary variable 2
    LC_check <- LC_check1/LC_check2                        # calculate the changing rate
    LC_check[ is.na( LC_check ) ] <- 1                     # assign na to 1 (no change)

    # stop if the changing rate out of the tolerance boundaries
    if( any( LC_check < (1 - LANDTOLERANCE) |
             LC_check > (1 + LANDTOLERANCE) ) )
      { stop( "ERROR: Interannual fluctuation in global land cover exceeds tolerance threshold" ) }


    #Write out the totals, by region and by region x GLU
    L125.LC_bm2_R_Yh_GLU %>%
      filter(year==1700) %>%                               # using the starting year only
      group_by(GCAM_region_ID, GLU) %>%                    # group by GCAM_region_ID and GLU
      summarise(value = sum(value)) %>%                    # summarize area by region and GLU
      rename(LC_bm2=value) ->                              # rename the column
      L125.LC_bm2_R_GLU

    L125.LC_bm2_R_Yh_GLU %>%
      filter(year==1700) %>%                               # using the starting year only
      group_by(GCAM_region_ID) %>%                         # group by GCAM_region_ID
      summarise(value = sum(value)) %>%                    # summarize area by region
      rename(LC_bm2=value) %>%                             # rename the column
      mutate(LC_bm2 = round(LC_bm2, DIGITS_LAND_TOTAL)) ->                 # keep the data with 2 digit precision
      L125.LC_bm2_R

    L125.LC_bm2_R_LT_Yh_GLU %>%                           # Land cover totals differentiated by land use types
      mutate(value = round(value, DIGITS_LAND_USE)) %>%                 # round totals to specified number of digits
      spread(key="year",value=value) ->
      L125.LC_bm2_R_LT_Yh_GLU

    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
    L125.LC_bm2_R %>%
      add_title("Total land cover by GCAM region") %>%
      add_units("bm2") %>%
      add_comments("Aggregated Land area by GCAM region") %>%
      add_comments("Rounded to 2 digit") %>%
      add_legacy_name("L125.LC_bm2_R") %>%
      add_precursors("temp-data-inject/L122.LC_bm2_R_HarvCropLand_Yh_GLU",
                     "temp-data-inject/L122.LC_bm2_R_OtherArableLand_Yh_GLU",
                     "temp-data-inject/L123.LC_bm2_R_MgdPast_Yh_GLU",
                     "temp-data-inject/L123.LC_bm2_R_MgdFor_Yh_GLU",
                     "temp-data-inject/L124.LC_bm2_R_Shrub_Yh_GLU_adj",
                     "temp-data-inject/L124.LC_bm2_R_Grass_Yh_GLU_adj",
                     "temp-data-inject/L124.LC_bm2_R_UnMgdPast_Yh_GLU_adj",
                     "temp-data-inject/L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj",
                     "L120.LC_bm2_R_UrbanLand_Yh_GLU",
                     "L120.LC_bm2_R_Tundra_Yh_GLU",
                     "L120.LC_bm2_R_RckIceDsrt_Yh_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L125.LC_bm2_R
    L125.LC_bm2_R_GLU %>%
      add_title("Total land cover by GCAM region and GLU") %>%
      add_units("bm2") %>%
      add_comments("Aggregated Land area by GCAM region x GLU") %>%
      add_legacy_name("L125.LC_bm2_R_GLU") %>%
      add_precursors("temp-data-inject/L122.LC_bm2_R_HarvCropLand_Yh_GLU",
                     "temp-data-inject/L122.LC_bm2_R_OtherArableLand_Yh_GLU",
                     "temp-data-inject/L123.LC_bm2_R_MgdPast_Yh_GLU",
                     "temp-data-inject/L123.LC_bm2_R_MgdFor_Yh_GLU",
                     "temp-data-inject/L124.LC_bm2_R_Shrub_Yh_GLU_adj",
                     "temp-data-inject/L124.LC_bm2_R_Grass_Yh_GLU_adj",
                     "temp-data-inject/L124.LC_bm2_R_UnMgdPast_Yh_GLU_adj",
                     "temp-data-inject/L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj",
                     "L120.LC_bm2_R_UrbanLand_Yh_GLU",
                     "L120.LC_bm2_R_Tundra_Yh_GLU",
                     "L120.LC_bm2_R_RckIceDsrt_Yh_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L125.LC_bm2_R_GLU
    L125.LC_bm2_R_LT_Yh_GLU %>%
      add_title("Total land cover by GCAM region / land type / historical year / GLU") %>%
      add_units("bm2") %>%
      add_comments("Data was read in from multiple data sources for each land cover type") %>%
      add_comments("Rounded to 5 digit") %>%
      add_legacy_name("L125.LC_bm2_R_LT_Yh_GLU") %>%
      add_precursors("temp-data-inject/L122.LC_bm2_R_HarvCropLand_Yh_GLU",
                     "temp-data-inject/L122.LC_bm2_R_OtherArableLand_Yh_GLU",
                     "temp-data-inject/L123.LC_bm2_R_MgdPast_Yh_GLU",
                     "temp-data-inject/L123.LC_bm2_R_MgdFor_Yh_GLU",
                     "temp-data-inject/L124.LC_bm2_R_Shrub_Yh_GLU_adj",
                     "temp-data-inject/L124.LC_bm2_R_Grass_Yh_GLU_adj",
                     "temp-data-inject/L124.LC_bm2_R_UnMgdPast_Yh_GLU_adj",
                     "temp-data-inject/L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj",
                     "L120.LC_bm2_R_UrbanLand_Yh_GLU",
                     "L120.LC_bm2_R_Tundra_Yh_GLU",
                     "L120.LC_bm2_R_RckIceDsrt_Yh_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L125.LC_bm2_R_LT_Yh_GLU

    return_data(L125.LC_bm2_R, L125.LC_bm2_R_GLU, L125.LC_bm2_R_LT_Yh_GLU)
  } else {
    stop("Unknown command")
  }
}
