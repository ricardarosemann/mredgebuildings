#' Calculation and Projection of Final to Useful Energy
#'
#' Calculate Efficiencies of Final (FE) to Useful (UE) Energy Conversion for all
#' combinations of Energy Carriers and Enduses. Missing data points within the
#' time interval of the original data (PFUDB) are filled with estimates from a
#' non-linear regression.
#' The efficiency projections are based on a model by De Stercke et al. which is
#' mainly driven by GDP per Capita. It describes an S-shaped curve approaching
#' assumed efficiency levels. The parameters of that curve are derived by a
#' regression with observations of IEA data.
#'
#' @param gasBioEquality Determines if natural gas and modern biomass share the same efficiencies
#'
#' @references De Stercke, S. (2014). Dynamics of Energy Systems: A Useful
#' Perspective (Interim Report, p. 68). IIASA.
#' http://pure.iiasa.ac.at/id/eprint/11254/1/IR-14-013.pdf
#'
#' @author Hagen Tockhorn
#'
#' @importFrom stats SSasymp na.omit
#' @importFrom dplyr reframe mutate select left_join rename group_by across all_of ungroup
#' filter semi_join case_when group_modify
#' @importFrom tidyr spread unite replace_na pivot_wider
#' @importFrom madrat calcOutput
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom magclass as.magpie
#'
#' @export


calcFEUEefficiencies <- function(gasBioEquality = TRUE) {


  # READ-IN DATA ---------------------------------------------------------------

  pfu <- calcOutput("PFUDB", aggregate = FALSE) %>%
    as.quitte()

  gdppop <- calcOutput("GDPpc",
                       scenario = "SSP2",
                       average2020 = FALSE,
                       unit = "constant 2005 Int$PPP",
                       aggregate = FALSE,
                       years = 1960:2022) %>%
    setNames("gdppop in constant 2005 Int$PPP") %>%
    as.quitte()

  # Efficiency regression parameters
  regPars <- calcOutput("EfficiencyRegression",
                        gasBioEquality = gasBioEquality,
                        aggregate = FALSE) %>%
    as.quitte()

  # Final energy weights
  feWeights <- calcOutput("FEbyEUEC", aggregate = FALSE) %>%
    as.quitte()



  # PROCESS DATA ---------------------------------------------------------------

  # Upper temporal boundary of data set
  maxPeriod <- max(pfu$period)


  # Replace vanishing demands with NA's to facilitate correction factor calculation
  pfu <- pfu %>%
    mutate(value = ifelse(.data[["value"]] == 0, NA, .data[["value"]]))


  # Combine with GDP per Cap for Period 1990-2020
  data <- pfu %>%
    interpolate_missing_periods(period = seq(1990, maxPeriod)) %>%
    left_join(gdppop %>%
                select(-"model", -"scenario", -"unit", -"variable") %>%
                rename(gdppop = "value"),
              by = c("region", "period")) %>%
    select("region", "period", "unit", "carrier", "enduse", "gdppop", "value")



  #--- Calculate Efficiency Estimates

  # Regression Parameters for enduse.carrier Combinations
  regPars <- regPars %>%
    select("variable", "carrier", "enduse", "value") %>%
    pivot_wider(names_from = "variable", values_from = "value")


  # Historical Efficiencies
  histEfficiencies <- data %>%
    # Calculate historical efficiencies
    pivot_wider(names_from = "unit", values_from = "value") %>%
    mutate(efficiency = .data[["ue"]] / .data[["fe"]]) %>%

    # Calculate missing efficiencies from regression parameters
    left_join(regPars, by = c("enduse", "carrier")) %>%
    group_by(across(all_of(c("carrier", "enduse")))) %>%
    mutate(pred = SSasymp(.data[["gdppop"]], .data[["Asym"]], .data[["R0"]], .data[["lrc"]])) %>%
    ungroup() %>%
    select(-"Asym", -"R0", -"lrc", -"fe", -"ue")


  #--- Match predictions with existing historical data points

  # Correction factor to adjust projections
  correctionFactors <- histEfficiencies %>%
    mutate(factor = .data[["efficiency"]] / .data[["pred"]]) %>%
    select(-"gdppop", -"efficiency", -"pred") %>%
    group_by(across(all_of(c("region", "enduse", "carrier")))) %>%

    # Linearly extrapolate factors for all periods
    group_modify(~ extrapolateMissingPeriods(.x, key = "factor", slopeOfLast = 5)) %>%
    ungroup()


  # NOTE: Countries missing the entire period range for a EC-EU-combination
  #       will be filled-up w/ non-corrected efficiency projections.
  histEfficiencies <- histEfficiencies %>%
    left_join(correctionFactors, by = c("region", "period", "enduse", "carrier")) %>%
    mutate(value = ifelse(is.na(.data[["factor"]]),
                          .data[["pred"]],
                          ifelse(is.na(.data[["efficiency"]]),
                                 .data[["pred"]] * .data[["factor"]],
                                 .data[["efficiency"]]))) %>%
    select("region", "period", "enduse", "carrier", "value") %>%
    filter(!is.na(.data[["value"]]))


  # FE weights for regional aggregation
  feWeights <- histEfficiencies %>%
    select("region", "period", "enduse", "carrier") %>%
    left_join(feWeights %>%
                filter(.data[["unit"]] == "fe") %>%
                select("region", "period", "enduse", "carrier", "value"),
              by = c("region", "period", "enduse", "carrier")) %>%
    interpolate_missing_periods(expand.values = TRUE) %>%
    group_by(across(all_of(c("period", "enduse", "carrier")))) %>%
    mutate(hasAnyData = any(!is.na(.data[["value"]])),
           value = case_when(!hasAnyData ~ 1 / n(), # Equal weights if no historical data exists
                             TRUE ~ replace_na(.data[["value"]], 0)), # Otherwise replace NA with 0
           # If sum is 0 or all values are NA, use equal weights
           value = ifelse(sum(.data[["value"]], na.rm = TRUE) == 0,
                          1 / n(),
                          .data[["value"]] / sum(.data[["value"]], na.rm = TRUE))) %>%
    ungroup() %>%
    select(-"hasAnyData")



  # OUTPUT ---------------------------------------------------------------------

  feWeights <- feWeights %>%
    as.quitte() %>%
    as.magpie()

  histEfficiencies <- histEfficiencies %>%
    as.quitte() %>%
    as.magpie()

  return(list(x = histEfficiencies,
              weight = feWeights,
              unit = "",
              description = "Historical Conversion Efficiencies from FE to UE",
              aggregationArguments = list(zeroWeight = "allow")))
}
