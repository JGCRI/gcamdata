#install.packages("lhs")

modify_A44.share_serv_fuel <- function(command, ...) {
  if(command == driver.DECLARE_MODIFY) { # objects that will be modified by the function
    return(c(FILE = "energy/A44.share_serv_fuel"))
  } else if(command == driver.DECLARE_INPUTS) { # objects that are inputted from the data system, but not modified
    return(c())
  } else if(command == driver.MAKE) {

    A44.share_serv_fuel <- get_data(load_csv_files("energy/A44.share_serv_fuel", optionals = F), 'energy/A44.share_serv_fuel')

    set.seed(SEED_NUM)

    A44.share_serv_fuel <- A44.share_serv_fuel %>%
      filter(share_TFEbysector != 0) %>%
      # sample by group
      group_by(region_GCAM3, sector) %>%
      mutate(LHS = lhs::randomLHS(1, dplyr::n())[1,] ) %>%
      ungroup %>%
      # convert to increase/decrease, within +- 0.1 of current value, but between 0.00001 and 1
      mutate(adder = 0.2 * LHS - 0.1,
             share_LHS_unscaled = pmin(pmax(0.00001, share_TFEbysector + adder), 1)) %>%
      # scale total to 1
      group_by(region_GCAM3, sector) %>%
      mutate(share_TFEbysector_scaled = share_LHS_unscaled / sum(share_LHS_unscaled) ) %>%
      ungroup %>%
      select(-share_TFEbysector, -adder, -share_LHS_unscaled, -LHS) %>%
      rename(share_TFEbysector = share_TFEbysector_scaled)

    # Produce outputs
    return_modified("energy/A44.share_serv_fuel" = A44.share_serv_fuel)
  } else {
    stop("Unknown command")
  }
}

devtools::load_all()


for (i in 1:3){
  drake::clean(list="modify_A44.share_serv_fuel")
  SEED_NUM <- i

  driver_drake(user_modifications = c("modify_A44.share_serv_fuel"),
               xml_suffix = paste0("__", i))
}


