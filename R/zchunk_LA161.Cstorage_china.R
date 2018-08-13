#' module_gcam.china_LA161.Cstorage
#'
#' Calculates onshore CO2 storage by grid region.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L161.Cstorage_province}. The corresponding file in the
#' original data system was \code{LA161.Cstorage.R} (gcam-china level1).
#' @details Calculates onshore CO2 storage by grid region.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author Liu August 2018

module_gcam.china_LA161.Cstorage <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/province_names_mappings",
             FILE = "gcam-china/CCS_provincial_data",
             FILE = "gcam-china/CCS_provincial_data_add",
             FILE = "gcam-china/unallocated_CStorage_province"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L161.Cstorage_province"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    province <- grid_region <- Cost_2005USDtCO2 <- CO2_Mt <- MtC <- Cumul_MtC <- quantile <-
      grade <- Cost_1990USDtC <- NULL

    all_data <- list(...)[[1]]

    # -----------------------------------------------------------------------------
    # 1.Load required inputs
    province_names_mappings <- get_data(all_data, "gcam-china/province_names_mappings")
    Dooley_CCS_China        <- get_data(all_data, "gcam-china/CCS_provincial_data")
    CCS_China_add           <- get_data(all_data, "gcam-china/CCS_provincial_data_add")
    Unallocated_CStorage    <- get_data(all_data, "gcam-china/unallocated_CStorage_province")

    # -----------------------------------------------------------------------------
    # 2.perform computations
    L161.Csupply_province <- Dooley_CCS_China %>%
      # Add grid.region
      left_join_error_no_match(province_names_mappings, by = "province.name") %>%
      # Convert prices to 1990 USD per ton C and amounts to C
      mutate(Cost_1990USDtC = Cost_2005USDtCO2 * emissions.CONV_C_CO2 / gdp_deflator(2005, 1990),
             MtC = CO2_Mt * (1 / emissions.CONV_C_CO2)) %>%
      arrange(province.name, Cost_2005USDtCO2) %>%
    # Removing data for provinces with not enough onshore storage
      filter(!(province.name %in% c("Fujian", "Guangdong", "Guangxi", "Qinghai")))

    province_data <- L161.Csupply_province %>%
      group_by(province.name) %>%
      # Calculate cumulative sum, then filter to the quantiles
      mutate(Cumul_MtC = cumsum(MtC))

    L161.Cstorage_province <- province_data %>%
      # use "type=1" to indicate that we want to return only exact matches to the data in the table.
      # only take the (0, 0.2, 0.4, 0.6, 0.8, 1) points in the quartiles
      filter(Cumul_MtC %in% quantile(Cumul_MtC, c(0, 0.2, 0.4, 0.6, 0.8, 1), type = 1)) %>%
      group_by(province.name) %>%
      mutate(# this will create grade equal to grade 1 for 0, grade 2 for 0.2, etc.
             grade = paste("grade",1:6, sep = " "),
             # from the cumulative totals, return to the marginal quantities associated with each grade
             MtC = Cumul_MtC- lag(Cumul_MtC,1,0)) %>%
      ungroup %>%
      # Return from the list to a data frame with all necessary data
      select(province.name, grade, Cost_1990USDtC, MtC) %>%
      # Add cost grades to provinces without data
      bind_rows(CCS_China_add) %>%
      mutate(# Setting a minimum cost of 0 on CO2 storage and transport projects
             Cost_1990USDtC = pmax(Cost_1990USDtC, 0))

    # Manually changing cost curves to avoid grades 2 or 3 being 0. Value determined by linear interpolation. If the data changes in the future, will need to change this
    L161.Cstorage_province$Cost_1990USDtC[which(L161.Cstorage_province$province.name== "Jilin" & L161.Cstorage_province$grade== "grade 2")]    <- 4.625
    L161.Cstorage_province$Cost_1990USDtC[which(L161.Cstorage_province$province.name== "Xinjiang" & L161.Cstorage_province$grade== "grade 2")] <- 4.3
    L161.Cstorage_province$Cost_1990USDtC[which(L161.Cstorage_province$province.name== "Yunnan" & L161.Cstorage_province$grade== "grade 2")]   <- 5.74
    L161.Cstorage_province$Cost_1990USDtC[which(L161.Cstorage_province$province.name== "Yunnan" & L161.Cstorage_province$grade== "grade 3")]   <- 11.48

    #Extending the plateau (bin 2) of the cost curve using additional resource data
    L161.Cstorage_province_g2 <- L161.Cstorage_province %>%
      filter(grade == "grade 2") ->
      L161.Cstorage_province_g2

    L161.Cstorage_province_g2 %>%
      mutate(add_storage = left_join_error_no_match(L161.Cstorage_province_g2, Unallocated_CStorage, by = c("province.name" = "province.name"))$MtCO2,
             #Convert to MtC for additional resource
             add_storage = add_storage * (1 / emissions.CONV_C_CO2)) ->
      L161.Cstorage_province_g2

    #Gansu is a special case, we are not adding storage
    L161.Cstorage_province_g2$add_storage[which(L161.Cstorage_province_g2$province.name=="Gansu")] <- 0

    L161.Cstorage_province_g2 %>%
      transform(MtC=as.numeric(MtC)) %>%
      mutate(MtC = MtC + add_storage) %>%
      select(province.name, grade, Cost_1990USDtC, MtC) ->
      L161.Cstorage_province_g2

    L161.Cstorage_province %>%
      filter(grade !="grade 2") %>%
      bind_rows(L161.Cstorage_province_g2) %>%
      mutate(province.name_0 = province.name) %>%
      arrange(province.name, grade) %>%
      map_province_name(province_names_mappings, "province", TRUE) %>%
      mutate(province.name = province.name_0) %>%
      select(province, province.name, grade, Cost_1990USDtC, MtC) ->
      L161.Cstorage_province

    # ===================================================
    # Produce outputs
    L161.Cstorage_province %>%
      add_title("CO2 storage curves by province and grade") %>%
      add_units("MtC; 1990 USD per tC") %>%
      add_comments("Cumulative MtC calculated by grid region, then filtered to quartiles") %>%
      add_legacy_name("L161.Cstorage_province") %>%
      add_precursors("gcam-china/province_names_mappings", "gcam-china/CCS_provincial_data", "gcam-china/unallocated_CStorage_province","gcam-china/CCS_provincial_data_add") %>%
      add_flags(FLAG_PROTECT_FLOAT) ->
      L161.Cstorage_province

    return_data(L161.Cstorage_province)
  } else {
    stop("Unknown command")
  }
}
