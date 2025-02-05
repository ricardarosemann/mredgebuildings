#' Historical residential floor space demand
#'
#' Data for many European countries is taken from EEA, India from IEA and other
#' countries from Daioglou et al 2012. The result does not cover all countries
#' and has mixed points in time depending on the region.
#'
#' @note RK: In Antoine's EDGE-B, data points associated with an GDP/POP above
#' 70000 USD/cap are dropped here to improve the later regression. This
#' filtering should be moved to getFloorspaceResidential where the regression is
#' performed. Therefore, the high-income data points are kept at this stage.
#'
#' @returns MAgPIE object with historic floor space
#'
#' @author Robin Hasse, Antoine Levesque, Hagen Tockhorn
#'
#' @importFrom madrat readSource calcOutput toolCountryFill
#' @importFrom quitte as.quitte calc_addVariable factor.data.frame interpolate_missing_periods
#' @importFrom dplyr filter mutate select anti_join group_by left_join %>%
#' ungroup .data %>% group_modify
#' @importFrom rlang .data
#' @importFrom magclass mbind as.magpie collapseDim mselect
#' @importFrom tidyr spread replace_na
#'
#' @export

calcFloorspacePast <- function() {

  # FUNCTIONS ------------------------------------------------------------------

  # specific floor space from absolute floor space and population
  floorPerCap <- function(floor, pop) {
    floor %>%
      rbind(pop) %>%
      calc_addVariable(`specific floor space` = "`floor space` / population",
                       units = "m2/cap", only.new = TRUE) %>%
      mutate(demographic = "Total")
  }


  # Predict missing historic floorspace data
  #
  # A linear regression is performed to establish a general relationship between
  # floorspace per capita and gdp per capita and population density.
  # Predicted values are corrected w.r.t. to historical data where available,
  # otherwise the prediction is chosen.
  makeFloorspaceProjection <- function(df, gdppop, dens, endOfHistory, periodBegin) {

    # Clean data
    gdppop <- gdppop %>%
      select(-"model", -"scenario", -"variable", -"unit") %>%
      rename(gdppop = "value")

    dens <- dens %>%
      select(-"model", -"scenario", -"variable", -"unit", -"data") %>%
      rename(density = "value")


    # create full data set
    dataFull <- df %>%
      filter(.data[["demographic"]] == "Total") %>%
      factor.data.frame() %>%
      interpolate_missing_periods(period = seq(periodBegin, endOfHistory)) %>%
      as.magpie() %>%
      toolCountryFill() %>%
      as.quitte() %>%
      droplevels() %>%
      left_join(gdppop, by = c("region", "period")) %>%
      left_join(dens, by = c("region", "period"))

    # estimation data set
    dataEstimate <- dataFull %>%
      filter(!is.na(.data[["value"]]))

    # make linear regression to obtain estimate
    estimate <- lm(log(value) ~ 1 + log(gdppop) + log(density), data = dataEstimate)

    # predict missing data
    dataPred <- dataFull %>%

      # make prediction with regressed parameters
      mutate(pred = exp(predict(estimate, newdata = dataFull))) %>%

      # create correction factor to balance-out deviations w.r.t. historic data
      group_by(across(all_of("region"))) %>%
      mutate(factor = .data[["value"]] / .data[["pred"]]) %>%

      # regress deviation factor for all periods
      group_modify(~ extrapolateMissingPeriods(.x, key = "factor", slopeOfLast = 20)) %>%
      ungroup() %>%

      # correct prediction deviations if factor is available
      mutate(pred = .data[["pred"]] * replace_na(.data[["factor"]], 1)) %>%

      # fill missing values w/ predictions
      mutate(value = ifelse(is.na(.data[["value"]]), .data[["pred"]], .data[["value"]])) %>%

      # select columns
      select("region", "period", "variable", "unit", "demographic", "value")

    return(dataPred)
  }


  # PARAMETERS -----------------------------------------------------------------

  # lower temporal boundary for historical data
  periodBegin <- 1990

  # upper temporal boundary for historical data
  endOfHistory <- 2020


  # LOAD AND CALCULATE DATA ----------------------------------------------------

  # data from Daioglou et al.
  daioglou <- readSource("Daioglou") %>%
    as.quitte(na.rm = TRUE) %>%
    filter(.data[["quintile"]] == 0) %>%
    select(-"quintile") %>%
    mutate(variable = "specific floor space",
           unit = "m2/cap")

  # EEA data: drop ESP and PRT (too high uncertainty)
  eea <- readSource("EEAfloorspace") %>%
    as.quitte(na.rm = TRUE) %>%
    filter(!.data[["region"]] %in% c("ESP", "PRT")) %>%
    mutate(variable = "floor space",
           unit = "million m2")

  # IEA data: take only India
  ind <- readSource("TCEP", subtype = "floorspace", convert = FALSE) %>%
    as.quitte() %>%
    filter(.data[["region"]] == "India",
           .data[["period"]] %in% c(2000, 2011),
           .data[["variable"]] == "Residential") %>%
    mutate(variable = "floor space",
           value = .data[["value"]] * 1000, # billion m2 -> million m2
           unit = "million m2",
           region = "IND")

  # historic population
  pop <- calcOutput("Population", scenario = "SSP2", aggregate = FALSE) %>%
    as.quitte() %>%
    filter(.data[["period"]] <= endOfHistory) %>%
    mutate(unit = "million cap",
           variable = gsub("pop_SSP2", "population", .data[["variable"]], fixed = TRUE))

  # historic GDP per capita
  gdppop <- calcOutput("GDPpc",
                       scenario = "SSP2",
                       average2020 = FALSE,
                       unit = "constant 2005 Int$PPP",
                       aggregate = FALSE,
                       years = 1960:endOfHistory) %>%
    setNames("gdppop") %>%
    as.quitte() %>%
    dplyr::mutate(region = droplevels(.data[["region"]]),
                  unit = as.factor("USD2005/cap"),
                  variable = as.character(.data[["variable"]]))

  # share of urban population
  urbanshare <- calcOutput("UrbanPast", aggregate = FALSE) %>%
    as.quitte() %>%
    mutate(variable = "urbanPop")

  # population density
  dens <- calcOutput("Density", aggregate = FALSE) %>%
    as.quitte()



  # PROCESS DATA ---------------------------------------------------------------

  # compute specific floor space
  eea <- floorPerCap(eea, pop)
  ind <- floorPerCap(ind, pop)

  # bind datasets
  data <- rbind(eea, ind)
  data <- data %>%
    rbind(anti_join(daioglou, data, by = c("period", "region", "demographic",
                                           "scenario", "variable")))

  # if missing, compute total from urban and rural data
  data <- data %>%
    group_by(across(all_of(c("region", "period")))) %>%
    filter(!any(.data[["demographic"]] == "Total"),
           any(.data[["demographic"]] == "Rural"),
           any(.data[["demographic"]] == "Urban")) %>%
    ungroup() %>%
    unite("variable", "variable", "demographic") %>%
    rbind(urbanshare) %>%
    as.quitte() %>%
    calc_addVariable(`specific floor space` =
                       "`specific floor space_Urban` * urbanPop +
                        `specific floor space_Rural` * (1 - urbanPop)",
                     units = "m2/cap", only.new = TRUE) %>%
    mutate(demographic = "Total") %>%
    rbind(data)

  # remove urban and rural data and extrapolate missing entries
  data <- data %>%
    filter(.data[["demographic"]] == "Total") %>%
    makeFloorspaceProjection(gdppop, dens, endOfHistory, periodBegin) %>%
    mutate(scenario = "history")



  # OUTPUT ---------------------------------------------------------------------

  data <- data %>%
    as.quitte() %>%
    as.magpie() %>%
    collapseDim()

  pop <- pop %>%
    as.quitte() %>%
    as.magpie() %>%
    mselect(region = getItems(data, 1), period = getItems(data, 2)) %>%
    collapseDim()


  return(list(x = data,
              weight = pop,
              min = 0,
              unit = "m2/cap",
              description = "floor space per capita"))
}
