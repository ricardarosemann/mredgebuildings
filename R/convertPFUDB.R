#' Converts Data from Primary Fuel and Useful Energy Data Base (PFUDB)
#'
#' This was adapted from EDGE function 'getPFUDB.R'.
#'
#' @param x MAgPIE object with data from PFUDB #nolint
#'
#' @returns magpie object
#'
#' @author Hagen Tockhorn
#'
#' @importFrom utils read.csv
#' @importFrom dplyr %>% rename
#' @importFrom rlang .data
#' @importFrom tidyr unite
#' @importFrom quitte as.quitte aggregate_map factor.data.frame
#' @importFrom magclass as.magpie
#' @importFrom madrat calcOutput toolGetMapping
#' @importFrom mrcommons toolSplitBiomass
#' @export


convertPFUDB <- function(x) {
  # READ-IN DATA ---------------------------------------------------------------

  data <- x

  # Get Weights
  ieaFe <- calcOutput("IEAPFU", aggregate = FALSE) %>%
    as.quitte()

  # Get Mapping
  regionmapping <- toolGetMapping("pfu_regionmapping.csv",
                                  type = "regional", where = "mredgebuildings")

  # Get GDP per Cap
  gdppop <- calcOutput("GDPpc",
                       scenario = "SSP2",
                       average2020 = FALSE,
                       aggregate = FALSE)


  # PARAMETERS -----------------------------------------------------------------

  # Energy Unit Conversion [TJ -> EJ]
  TJ2EJ <- 1e-6 #nolint


  # PROCESS DATA ---------------------------------------------------------------

  # Make columns lowercase
  colnames(data) <- tolower(colnames(data))

  data <- data %>%
    as.quitte() %>%
    factor.data.frame()


  # Match Periods
  pfu <- data %>%
    as.quitte() %>%
    filter(.data[["period"]] %in% getPeriods(ieaFe)) %>%
    mutate(value = replace_na(.data[["value"]], 0))

  ieaFe <- ieaFe %>%
    as.quitte() %>%
    filter(.data[["period"]] %in% getPeriods(pfu)) %>%
    mutate(value = replace_na(.data[["value"]], 0))


  # Filter out Uses where all Values for Region/Period are vanishing
  pfu <- pfu %>%
    group_by(.data[["carrier"]]) %>%
    filter(sum(abs(.data[["value"]])) != 0) %>%
    ungroup() %>%
    group_by(.data[["use"]]) %>%
    filter(sum(abs(.data[["value"]])) != 0) %>%
    ungroup()

  # Temporal Renaming for aggregate_map to work well
  pfu <- pfu %>%
    select(-"variable") %>%
    rename(variable = "carrier")


  # DISAGGREGATE ---------------------------------------------------------------

  pfu <- pfu %>%
    droplevels() %>%
    aggregate_map(mapping = regionmapping[!is.na(regionmapping$PFUDB), ],
                  by = c("region" = "PFUDB"),
                  subset2agg = levels(pfu$variable),
                  weights = ieaFe %>%
                    rename(weight = "value") %>%
                    select(-"model", -"scenario", -"unit") %>%
                    droplevels(),
                  weight_item_col = "region",
                  weight_val_col = "weight") %>%
    mutate(value = replace_na(.data[["value"]], 0)) %>%
    as.quitte() %>%
    as.magpie() %>%
    toolSplitBiomass(gdppop, "Biomass", dim = "variable") %>%
    as.quitte()

  # Adaptation of correct Format
  pfu <- pfu %>%
    mutate(value = .data[["value"]] * TJ2EJ) %>%
    rename(carrier = "variable",
           enduse = "use") %>%
    select(c("region", "period", "carrier", "enduse", "unit", "value"))


  # OUTPUT -------------------------------------------------------------------

  pfu <- pfu %>%
    factor.data.frame() %>%
    droplevels() %>%
    as.magpie() %>%
    toolCountryFill()

  return(pfu)
}
