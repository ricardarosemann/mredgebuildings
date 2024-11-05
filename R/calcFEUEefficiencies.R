#' Calculation and Projection of Final to Useful Energy
#'
#' Calculate Efficiencies of Final (FE) to Useful (UE) Energy Conversion for all
#' combinations of Energy Carriers and Enduses.
#' The efficiency projections are based on a model by De Stercke et al. which is
#' mainly driven by GDP per Capita. It describes an S-shaped curve approaching
#' assumed efficiency levels. The parameters of that curve are derived by a
#' regression with observations of IEA data.
#'
#' @param gasBioEquality Determines if carriers natgas and biomod share the same efficiencies
#'
#' @references De Stercke, S. (2014). Dynamics of Energy Systems: A Useful
#' Perspective (Interim Report, p. 68). IIASA.
#' http://pure.iiasa.ac.at/id/eprint/11254/1/IR-14-013.pdf
#'
#' @author Hagen Tockhorn
#'
#' @importFrom stats SSasymp na.omit
#' @importFrom dplyr reframe mutate select left_join rename group_by across all_of ungroup
#' filter semi_join
#' @importFrom tidyr spread unite replace_na
#' @importFrom madrat calcOutput toolGetMapping
#' @importFrom quitte as.quitte interpolate_missing_periods removeColNa
#' @importFrom magclass as.magpie
#'
#' @export


calcFEUEefficiencies <- function(endOfHistory = 2020, gasBioEquality = TRUE) {

  # READ-IN DATA ---------------------------------------------------------------

  pfu <- calcOutput("PFUDB", aggregate = FALSE) %>%
    as.quitte()

  gdppop <- calcOutput("GDPPop", aggregate = FALSE, endOfHistory = endOfHistory) %>%
    as.quitte()


  # regression parameter corrections
  parsCorr <- toolGetMapping(name  = "correct_efficiencies.csv",
                             type  = "sectoral",
                             where = "mredgebuildings")


  # PARAMETERS -----------------------------------------------------------------

  eqEffs <- c("water_heating.natgas" = "water_heating.biomod",
              "space_heating.natgas" = "space_heating.biomod",
              "cooking.natgas" = "cooking.biomod")


  # PROCESS DATA ---------------------------------------------------------------

  # Combine with GDP per Cap for Period 1990 until endOfHistory
  data <- pfu %>%
    interpolate_missing_periods(period = seq(1990, endOfHistory)) %>%
    mutate(value = ifelse(is.na(.data[["value"]]), 0, .data[["value"]])) %>%
    left_join(gdppop %>%
                select(-"model", -"scenario", -"unit", -"variable") %>%
                rename(gdppop = "value"),
              by = c("region", "period")) %>%
    select(-"model", -"scenario", -"variable")


  #--- Calculate Efficiency Estimates

  # Regression Parameters for enduse.carrier Combinations
  regPars <- calcOutput("EfficiencyRegression", aggregate = FALSE) %>%
    as.quitte() %>%
    spread(key = "variable", value = "value") %>%
    select(-"model", -"scenario", -"region", -"unit", -"period")

  # Assign Parameters to carrier-enduse Combination
  dataHist <- data %>%
    removeColNa() %>%
    left_join(regPars, by = c("carrier", "enduse")) %>%
    na.omit()


  # Predict Historic Efficiencies with Non-Linear Model
  dataHist <- dataHist %>%
    spread(key = "unit", value = "value") %>%
    mutate(efficiency = .data[["ue"]] / .data[["fe"]]) %>%
    group_by(across(all_of(c("carrier", "enduse")))) %>%
    mutate(pred = SSasymp(.data[["gdppop"]], .data[["Asym"]], .data[["R0"]], .data[["lrc"]])) %>%
    ungroup() %>%
    select(-"Asym", -"R0", -"lrc", -"fe", -"ue")


  #--- Match Region-Specific Curves to fill non-existing Data Points

  # Extract corrected carrier/enduse combinations
  euecCorr <- paste(parsCorr$enduse, parsCorr$carrier, sep = ".")

  # Create Correction Factor to adjust Projections
  corrFactors <- dataHist %>%
    mutate(factor = .data[["efficiency"]] / .data[["pred"]]) %>%
    select(-"gdppop", -"efficiency", -"pred") %>%
    interpolate_missing_periods(value = "factor", expand.values = TRUE) %>%

    # set correction factors to NA for corrected regression parameters
    mutate(factor = ifelse(paste(.data[["enduse"]], .data[["carrier"]], sep = ".") %in% euecCorr,
                           NA,
                           .data[["factor"]]))



  dataHist <- dataHist %>%
    left_join(corrFactors, by = c("region", "period", "enduse", "carrier")) %>%
    mutate(value = ifelse(is.na(.data[["factor"]]),
                          .data[["pred"]],
                          ifelse(is.infinite(.data[["factor"]]),
                                 .data[["pred"]],
                                 ifelse(is.na(.data[["efficiency"]]),
                                        .data[["pred"]] * .data[["factor"]],
                                        .data[["efficiency"]]))))


  # Trim Dataframe
  efficiencies <- dataHist %>%
    select(-"gdppop", -"efficiency", -"pred", -"factor") %>%
    mutate(scenario = "history")


  #--- Corrections

  # Biomod Efficiency identical to Natgas
  if (gasBioEquality) {
    gasEffs <- efficiencies %>%
      unite("variable", "enduse", "carrier", sep = ".") %>%
      filter(.data[["variable"]] %in% names(eqEffs))

    for (gasVar in names(eqEffs)) {
      bioEffs <- gasEffs %>%
        filter(.data[["variable"]] == gasVar) %>%
        mutate(variable = eqEffs[gasVar][[1]]) %>%
        separate("variable", into = c("enduse", "carrier"), sep = "\\.")

      efficiencies <- rbind(
        anti_join(efficiencies, bioEffs, by = c("region", "period", "enduse", "carrier", "scenario")),
        bioEffs
      )
    }
  }


  # FE Weights
  feWeights <- pfu %>%
    interpolate_missing_periods(period = seq(1990, endOfHistory), expand.values = TRUE) %>%
    filter(.data[["unit"]] == "fe") %>%
    semi_join(efficiencies, by = c("region", "period", "carrier", "enduse")) %>%
    group_by(across(all_of(c("period", "region")))) %>%
    reframe(value = sum(.data[["value"]], na.rm = TRUE)) %>%
    mutate(value = replace_na(.data[["value"]], 0)) %>%
    as.quitte() %>%
    as.magpie()


  # OUTPUT ---------------------------------------------------------------------

  # Weights = FE?

  efficiencies <- efficiencies %>%
    as.quitte() %>%
    as.magpie()

  return(list(x = efficiencies,
              weights = feWeights,
              min = 0,
              unit = "",
              description = "Historical Conversion Efficiencies from FE to UE"))
}
