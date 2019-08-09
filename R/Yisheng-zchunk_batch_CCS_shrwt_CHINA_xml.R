#' module_gcam.china_batch_CCS_shrwt_CHINA_xml
#'
#' Construct XML data structure for \code{CCS_shrwt_CHINA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{CCS_shrwt_CHINA.xml}. The corresponding file in the
#' original data system was \code{batch_CCS_shrwt_CHINA.xml} (gcam-china processing code-xml batch).
module_gcamchina_batch_CCS_shrwt_CHINA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L9999.StubTechShrwt_CHINA",
             "L9999.StubTechInterpOverwrite_CHINA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "CCS_shrwt_CHINA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L9999.StubTechShrwt_CHINA <- get_data(all_data, "L9999.StubTechShrwt_CHINA")
    L9999.StubTechInterpOverwrite_CHINA <- get_data(all_data, "L9999.StubTechInterpOverwrite_CHINA")

    # ===================================================

    # Produce outputs
    # Unknown function of L9999.StubTechInterpOverwrite_CHINA file, since it's not included in
    # final xml in previous data system @Yisheng Sun
    create_xml("CCS_shrwt_CHINA.xml") %>%
      add_xml_data(L9999.StubTechShrwt_CHINA, "StubTechShrwt") %>%
    # add_xml_data(L9999.StubTechInterpOverwrite_CHINA, "THIS HEADER CANNOT BE FOUND") %>%
      add_precursors("L9999.StubTechShrwt_CHINA") ->
      CCS_shrwt_CHINA.xml


    return_data(CCS_shrwt_CHINA.xml)
  } else {
    stop("Unknown command")
  }
}
