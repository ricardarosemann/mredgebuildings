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
#' @param endOfHistory Last historic time period
#'
#' @references De Stercke, S. (2014). Dynamics of Energy Systems: A Useful
#' Perspective (Interim Report, p. 68). IIASA.
#' http://pure.iiasa.ac.at/id/eprint/11254/1/IR-14-013.pdf
#'
#' @author Hagen Tockhorn
#'
#' @importFrom quitte aggregate_map removeColNa as.quitte
#' @importFrom stats as.formula na.omit nls
#' @importFrom dplyr mutate select filter left_join group_by across all_of rename
#' @importFrom tidyr spread unite
#' @importFrom madrat calcOutput toolGetMapping
#' @importFrom magclass as.magpie
#'
#' @export


calcEfficiencyRegression <- function(endOfHistory = 2020) {

  # FUNCTIONS ------------------------------------------------------------------

  # Extrapolate historic FE-UE Efficiencies from Fit Function
  calcPars <- function(df, var) {
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

  pfu <- calcOutput("PFUDB", aggregate = FALSE, endOfHistory = endOfHistory) %>%
    as.quitte()

  gdppop <- calcOutput("GDPPop", aggregate = FALSE) %>%
    as.quitte()

  # Get Mapping (ISO<->PFU)
  regionmapping <- toolGetMapping("pfu_regionmapping.csv", type = "regional", where = "mredgebuildings")

  # Get Population Data
  pop <- calcOutput("PopulationPast", aggregate = FALSE) %>%
    as.quitte()

  # regression parameter corrections
  parsCorr <- toolGetMapping(name  = "correct_efficiencies.csv",
                             type  = "sectoral",
                             where = "mredgebuildings")


  # PARAMETERS -----------------------------------------------------------------

  # Minimum Requirement to be considered
  minEfficiency <- 0.05


  # PROCESS DATA ---------------------------------------------------------------

  #--- Calculate existing FE-EU efficiencies

  # Aggregate PFU Data to PFU Country Code
  data <- pfu %>%
    mutate(value = replace_na(.data[["value"]], 0)) %>%
    unite("variable", "enduse", "carrier", sep = ".") %>%
    aggregate_map(mapping = regionmapping[!is.na(regionmapping$PFUDB), c("iso", "PFUDB")],
                  by = c("region" = "iso"),
                  forceAggregation = TRUE)

  # Aggregate GDPpop to PFU Country Code
  gdppop <- gdppop %>%
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
  data <- data %>%
    left_join(gdppop %>%
                rename(gdppop = "value"),
              by = c("region", "period"))

  # Calculate Efficiencies as Regression-Input
  data <- data %>%
    select(-"model", -"scenario") %>%
    spread(.data[["unit"]], .data[["value"]]) %>%
    mutate(value = .data[["ue"]] / .data[["fe"]]) %>%
    select(-"fe", -"ue")

  # Filter out unrealistic Efficiencies
  data <- filter(data, .data[["value"]] > minEfficiency)

  vars <- data %>%
    group_by(across(all_of("variable"))) %>%
    filter(.data[["variable"]] != "gdppop",
           !all(is.na(.data[["value"]]))) %>%
    getElement("variable") %>%
    unique()


  #--- Calculate Parameters
  parsFull <- data.frame()

  for (var in vars) {
    pars <- calcPars(data, var)
    parsFull <- as.data.frame(do.call(cbind, as.list(pars))) %>%
      mutate(variable = var) %>%
      rbind(parsFull)
  }

  parsFull <- parsFull %>%
    separate("variable", c("enduse", "carrier"), sep = "\\.")



  # CORRECTIONS ----------------------------------------------------------------

  # Rename corrected parameters
  parsCorr <- parsCorr %>%
    rename(AsymCorr = "Asym", lrcCorr = "lrc", R0Corr = "R0")


  # Correct Regression Parameters for electrical Heat Transfer Technologies
  parsFull <- parsFull %>%
    left_join(parsCorr, by = c("carrier", "enduse")) %>%
    mutate(Asym = ifelse(is.na(.data[["AsymCorr"]]), .data[["Asym"]], .data[["AsymCorr"]]),
           R0 = ifelse(is.na(.data[["R0Corr"]]), .data[["R0"]], .data[["R0Corr"]]),
           lrc = ifelse(is.na(.data[["lrcCorr"]]), .data[["lrc"]], .data[["lrcCorr"]])) %>%
    select(-"AsymCorr", -"R0Corr", -"lrcCorr")



  # OUTPUT ---------------------------------------------------------------------

  # Trim Dataframe
  parsFull <- parsFull %>%
    mutate(region = "GLO") %>%
    select("region", "carrier", "enduse", "Asym", "R0", "lrc") %>%
    as.magpie()


  return(list(x = parsFull,
              isocountries = FALSE,
              description = "Regression Parameter for FE-UE-Efficiency Projection",
              unit = ""))
}
