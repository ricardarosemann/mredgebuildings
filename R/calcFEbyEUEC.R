#' Historic Final Energy Demand by Carrier and Enduse
#'
#' Takes the historic final energy demand by carriers from IEA and disaggregates
#' it into different end uses.
#'
#' The disaggregation is performed such that the aggregated carrier-specific FE
#' values of the IEA IO regions are met as a minimum requirement. In an ideal case,
#' the enduse-specific FE shares are met as well.
#'
#' @note
#' For now, existing disaggregated final energy data with respect to carriers and
#' enduses combined is replaced in the final output. However, since the Odyssee
#' data is largely underestimating the real IEA FE targets, these shall serve as
#' lower boundaries for the disaggregation.
#' Such feature has been implemented in \code{toolDisaggregate} but is not running
#' smoothly yet.
#'
#' @returns data.frame with historic energy demands
#'
#' @author Hagen Tockhorn, Robin Hasse
#'
#' @importFrom dplyr mutate semi_join right_join left_join as_tibble filter
#'   select %>% .data pull
#' @importFrom madrat toolCountryFill calcOutput toolGetMapping
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @export

calcFEbyEUEC <- function() {

  # READ-IN DATA ---------------------------------------------------------------

  # FE Data
  ieaIO <- calcOutput("IOEdgeBuildings", subtype = "output_EDGE_buildings",
                      aggregate = FALSE) %>%
    as.quitte(na.rm = TRUE)


  # FE EU Data
  feOdyssee <- calcOutput("ShareOdyssee",
                          subtype = "enduse_carrier",
                          feOnly = TRUE,
                          aggregate = FALSE) %>%
    as.quitte()


  # FE IEA EEI data
  feIEAEEI <- calcOutput("IEA_EEI",
                         subtype = "buildings",
                         aggregate = FALSE) %>%
    as.quitte()


  # EU Shares
  sharesEU <- calcOutput("Shares",
                         subtype = "enduse_nonthermal",
                         aggregate = TRUE,
                         regionmapping = "regionmappingEUshares.csv") %>%
    as.quitte()


  # EU Shares mapping
  regmapping <- toolGetMapping(name  = "regionmappingEUshares.csv",
                               type  = "regional",
                               where = "mredgebuildings") %>%
    select("region", "regionAgg")



  # PARAMETERS -----------------------------------------------------------------

  # Enduse-Carrier combinations which will be systematically excluded
  exclude <- toolGetMapping(name  = "excludeEnduseCarrier.csv",
                            type  = "sectoral",
                            where = "mredgebuildings")


  # PROCESS DATA ---------------------------------------------------------------

  sharesEU <- sharesEU %>%
    select("region", "period", "enduse", "value")

  ieaIO <- ieaIO %>%
    rename(carrier = "variable") %>%
    semi_join(sharesEU, by = c("period"))


  # remove district space cooling from disaggregation
  exclude <- exclude %>%
    rbind(data.frame(enduse = "space_cooling",
                     carrier = "heat"))


  #--- Prepare toolDisaggregate Input

  # combine the already disaggregated data
  feDisagg <- feOdyssee %>%
    left_join(feIEAEEI,
              by = c("region", "period", "carrier", "enduse")) %>%
    mutate(value = ifelse(is.na(.data[["value.x"]]),
                          .data[["value.y"]],
                          .data[["value.x"]])) %>%
    select("region", "period", "carrier", "enduse", "value")


  # Disaggregate FE with EU/EC Shares
  ieaIODis <- ieaIO %>%
    select("region", "period", "carrier", "value") %>%
    mutate(unit = "fe") %>%
    toolDisaggregate(enduseShares  = sharesEU,
                     exclude       = exclude,
                     dataDisagg    = feDisagg,
                     regionMapping = regmapping,
                     outliers      = c("IND", "CHN", "ZAF")) %>%
    select("region", "period", "unit", "carrier", "enduse", "value")


  # existing enduse-carrier shares are applied directly on IEA data
  dataReplaceFull <- feOdyssee %>%
    select("region", "period", "carrier", "enduse", "value") %>%
    group_by(across(all_of(c("region", "carrier", "enduse")))) %>%
    filter(all(!is.na(.data[["value"]]))) %>%
    ungroup() %>%
    group_by(across(all_of(c("region", "period", "carrier")))) %>%
    mutate(share = proportions(.data[["value"]])) %>%
    ungroup() %>%
    select(-"value") %>%
    left_join(ieaIO, by = c("region", "period", "carrier")) %>%
    mutate(replaceValue = .data[["value"]] * .data[["share"]],
           replaceValue = replace_na(.data[["replaceValue"]], 0)) %>%
    select("region", "period", "carrier", "enduse", "replaceValue")


  # existing disaggregated data replaces values from optimization
  data <- ieaIODis %>%
    left_join(dataReplaceFull, by = c("region", "period", "carrier", "enduse")) %>%
    mutate(valueUncorrected = ifelse(is.na(.data[["replaceValue"]]),
                                     .data[["value"]],
                                     .data[["replaceValue"]])) %>%
    group_by(across(all_of(c("region", "period", "carrier")))) %>%
    mutate(value = ifelse(.data[["valueUncorrected"]] == 0,
                          0,
                          sum(.data[["value"]]) * proportions(.data[["valueUncorrected"]]))) %>%
    ungroup() %>%
    select(-"valueUncorrected") %>%
    select("region", "period", "unit", "carrier", "enduse", "value")


  # CORRECTIONS ----------------------------------------------------------------

  # Since the data on district cooling is very sparse and the technology
  # exhibits a low global penetration rate, we assume that all historic cooling
  # demand is covered by # electricity but assume that district cooling might
  # play a more significant role in the future.

  dataCorr <- data %>%
    select(-"enduse", -"carrier", -"value") %>%
    unique() %>%
    mutate(enduse = "space_cooling",
           carrier = "heat",
           value = 0)

  dataFull <- rbind(dataCorr, data)


  # Aggregate End-Uses appliances and lighting
  dataFull <- dataFull %>%
    aggToLevel(variables = c("appliances", "lighting"), level = "enduse", newname = "appliances_light")




  # OUTPUT ---------------------------------------------------------------------

  # Pack Data
  dataFull <- dataFull %>%
    mutate(scenario = "history") %>%
    as.quitte() %>%
    as.magpie() %>%
    toolCountryFill(0, verbosity = 2)


  return(list(x = dataFull,
              weight = NULL,
              min = 0,
              unit = "EJ",
              description = "Historic Final Energy Data from IEA disaggregated by end use"))
}
