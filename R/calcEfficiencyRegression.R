#' Calculation Regression Parameters for FE-UE Efficiency Projection
#'
#' Calculate the regression parameters for the projection of final (FE)
#' to useful (UE) energy conversion projection for all combinations of
#' energy enduses and carriers. The regression parameters correspond to an
#' asymptotic regression model and encompass the parameters Asym, R0 and lrc.
#' The are determined using a nonlinear least-squares regression.
#'
#' This approach closely follows the model by De Stercke et al. which is
#' mainly driven by GDP per Capita.
#'
#' @param gasBioEquality Determines if natural gas and modern biomass share the same efficiencies
#'
#' @references De Stercke, S. (2014). Dynamics of Energy Systems: A Useful
#' Perspective (Interim Report, p. 68). IIASA.
#' http://pure.iiasa.ac.at/id/eprint/11254/1/IR-14-013.pdf
#'
#' @author Hagen Tockhorn
#'
#' @importFrom quitte aggregate_map removeColNa as.quitte
#' @importFrom stats as.formula na.omit nls
#' @importFrom dplyr mutate select filter left_join group_by across all_of rename coalesce ends_with %>% .data
#' @importFrom tidyr spread unite
#' @importFrom madrat calcOutput toolGetMapping
#' @importFrom magclass as.magpie
#'
#' @export


calcEfficiencyRegression <- function(gasBioEquality = TRUE) {

  # FUNCTIONS ------------------------------------------------------------------

  # Extrapolate historic FE-UE Efficiencies from Fit Function
  getRegressionPars <- function(df, var) {
    # Prepare Historic Data
    dataHist <- df %>%
      removeColNa() %>%
      filter(.data[["variable"]] == var) %>%
      spread("variable", "value") %>%
      na.omit()

    # Replace 0's with small Values to avoid Inf Issues
    dataHist[dataHist[var] == 0, var] <-
      min(dataHist[dataHist[var] != 0, var]) / 10

    # Create Estimation Object for Non-Linear Model
    estimate <- nls(as.formula(paste(var, "~ SSasymp(gdppop, Asym, R0, lrc)")),
                    dataHist)

    return(estimate$m$getPars())
  }



  # READ-IN DATA ---------------------------------------------------------------

  # --- Data

  # Final and useful energy data
  pfu <- calcOutput("PFUDB", aggregate = FALSE) %>%
    as.quitte()

  # GDP per capita
  gdppop <- calcOutput("GDPpc",
                       scenario = "SSP2",
                       average2020 = FALSE,
                       aggregate = FALSE) %>%
    as.quitte()

  # Population
  pop <- calcOutput("PopulationPast", aggregate = FALSE) %>%
    as.quitte()


  # --- Mappings

  # Region mapping
  regionmapping <- toolGetMapping("pfu_regionmapping.csv", type = "regional", where = "mredgebuildings")

  # Regression parameter corrections
  parsCorrections <- toolGetMapping("correctEfficiencies.csv",
                                    type = "sectoral",
                                    where = "mredgebuildings")

  # Equal efficiency assumptions
  equalEfficiencies <- toolGetMapping("equalEfficiencies.csv",
                                      type = "sectoral",
                                      where = "mredgebuildings")



  # PARAMETERS -----------------------------------------------------------------

  # Minimum Requirement to be considered
  minEfficiency <- 0.05



  # PROCESS DATA ---------------------------------------------------------------

  #--- Calculate existing FE-EU efficiencies

  # Aggregate PFU Data to PFU Country Code
  pfuAggregate <- pfu %>%
    mutate(value = replace_na(.data[["value"]], 0)) %>%
    unite("variable", c("enduse", "carrier"), sep = ".") %>%
    aggregate_map(mapping = regionmapping[!is.na(regionmapping$PFUDB), c("iso", "PFUDB")],
                  by = c("region" = "iso"),
                  forceAggregation = TRUE)

  # Aggregate GDPpop to PFU Country Code
  gdppopAggregate <- gdppop %>%
    aggregate_map(mapping = regionmapping[!is.na(regionmapping$PFUDB), c("iso", "PFUDB")],
                  by = c("region" = "iso"),
                  forceAggregation = TRUE,
                  weights = pop %>%
                    select("region", "period", "value") %>%
                    rename(weight = "value"),
                  weight_item_col = "region",
                  weight_val_col = "weight") %>%
    select(-"model", -"scenario", -"unit", -"variable")


  # Combine with GDP per Cap
  data <- pfuAggregate %>%
    left_join(gdppopAggregate %>%
                rename(gdppop = "value"),
              by = c("region", "period"))


  histEfficiencies <- data %>%
    # Calculate efficiencies from UE/FE data
    select(-"model", -"scenario") %>%
    spread(.data[["unit"]], .data[["value"]]) %>%
    mutate(value = .data[["ue"]] / .data[["fe"]]) %>%
    select(-"fe", -"ue") %>%

    # Filter out unrealistic efficiencies
    filter(.data[["value"]] >= minEfficiency)


  euecCombinations <- unique(histEfficiencies$variable)


  #--- Calculate regression parameter
  fitPars <- do.call(rbind, lapply(euecCombinations, function(euec) {
    pars <- getRegressionPars(histEfficiencies, euec)
    as.data.frame(do.call(cbind, as.list(pars))) %>%
      mutate(variable = euec)
  }))



  # CORRECTIONS ----------------------------------------------------------------

  # Replace regression parameters to account for RH/HP mix in electric heating end-uses
  fitPars <- fitPars %>%
    left_join(parsCorrections, by = "variable", suffix = c("", ".corr")) %>%
    mutate(Asym = coalesce(.data[["Asym.corr"]], .data[["Asym"]]),
           R0   = coalesce(.data[["R0.corr"]],   .data[["R0"]]),
           lrc  = coalesce(.data[["lrc.corr"]],  .data[["lrc"]])) %>%
    select(-ends_with(".corr"))


  # Correct efficiencies of enduse.carrier combinations assumed to be of equal efficiency
  if (isTRUE(gasBioEquality)) {
    fitPars <- fitPars %>%
      left_join(equalEfficiencies, by = "variable") %>%
      left_join(fitPars,
                by = c("equalTo" = "variable"),
                suffix = c("", ".target")) %>%
      mutate(Asym = ifelse(!is.na(.data[["equalTo"]]), .data[["Asym.target"]], .data[["Asym"]]),
             R0   = ifelse(!is.na(.data[["equalTo"]]), .data[["R0.target"]],   .data[["R0"]]),
             lrc  = ifelse(!is.na(.data[["equalTo"]]), .data[["lrc.target"]],  .data[["lrc"]])) %>%
      select(-"equalTo", -ends_with(".target"))
  }



  # OUTPUT ---------------------------------------------------------------------

  # Trim Dataframe
  fitPars <- fitPars %>%
    separate("variable", c("enduse", "carrier"), sep = "\\.") %>%
    pivot_longer(cols = c("Asym", "R0", "lrc"), names_to = "variable", values_to = "value") %>%
    mutate(region = "GLO") %>%
    select("region", "carrier", "enduse", "variable", "value") %>%
    as.magpie()


  return(list(x = fitPars,
              isocountries = FALSE,
              description = "Regression Parameter for FE-UE-Efficiency Projection",
              unit = ""))
}
