#' Household and demand data from Daioglou et al. 2012
#'
#' Compilation of survey data. Quintile 0 stands for the entire population.
#' - households: Global data on population, household size, floor space per
#' capita (m2/cap), population density (cap/km2), GINI and household
#' expenditures (PPP2005/cap). Sources are written into a dimension to allow
#' later cleaning based on sources
#' - shares: Final energy shares of end uses in buildings. Sources are dropped.
#'
#' @author Robin Hasse, Antoine Levesque
#'
#' @references https://doi.org/10.1016/j.energy.2011.10.044
#'
#' @param subtype <dataFile>.<variable>
#'
#' @importFrom utils read.csv
#' @importFrom dplyr rename select mutate group_by summarise across %>% any_of
#'   .data
#' @importFrom tidyr replace_na
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie collapseDim
#' @export

readDaioglou <- function(subtype = "households.specific floor space") {

  # split subtype
  dataFile <- gsub("\\..*$", "", subtype)
  variable <- if (dataFile == subtype) {
    NULL
  } else {
    gsub("^.*\\.", "", subtype)
  }

  # remove separator and make string numeric
  asNum <- function(string) {
    as.numeric(gsub(",", "", string))
  }

  # pick file
  file <- switch(dataFile,
                 households          = "Household_Characteristics3.csv",
                 share_cooking       = "Cooking_Energy_Shares.csv",
                 share_water_heating = "Water_Heating_Shares.csv",
                 share_space_heating = "Space_Heating_Shares.csv",
                 share_lighting      = "Lighting_Shares.csv",
                 stop("'", dataFile, "' is not a valid data file."))

  # rename columns
  renameCols <- c(
    Country                            = "region",
    Year                               = "period",
    Demographic                        = "demographic",
    Quintile                           = "quintile",
    Population                         = "population",
    HHsize.cap.HH                      = "household size",
    Floorspace.cap.m.2.cap             = "specific floor space",
    Population.Density.cap.km.2        = "population density",
    GINI                               = "gini",
    Household.Expenditure.PPP.2005.Cap = "household expenditure",
    Coal                               = "coal",
    Traditional.Fuelwood               = "traditional fuelwood",
    Improved.Fuelwood                  = "improved fuelwood",
    Kerosene                           = "kerosine",
    LPG                                = "lpg",
    Fuel.Oil                           = "fuel oil",
    Natural.Gas                        = "natural gas",
    Candles                            = "candles",
    Biogas                             = "biogas",
    Electricity                        = "electricity",
    Remote.Heating                     = "remote heating",
    Other                              = "other",
    Sources                            = "source"
  )

  # read data
  data <- read.csv(file,
                   stringsAsFactors = FALSE,
                   encoding = "UTF-8",
                   row.names = if (dataFile == "households") NULL else 1)

  # drop irrelevant columns and rename remaining
  data <- data[, intersect(colnames(data), names(renameCols))]
  colnames(data) <- renameCols[colnames(data)]
  if (dataFile != "households") {
    data["source"] <- NULL
    data["household expenditure"] <- NULL
  }

  # tidy data
  if (dataFile == "households") {
    # TODO: Hack to remove non-UTF-8 character. Should solve this properly. #nolint
    data <- data %>%
      mutate(source = iconv(.data[["source"]], "UTF-8", "UTF-8", sub = ""))
    data <- data %>%
      mutate(population = asNum(.data[["population"]]),
             `population density` = asNum(.data[["population density"]]),
             `household expenditure` = asNum(.data[["household expenditure"]]),
             # TODO: Hack to remove non-UTF-8 character. Should solve this properly. #nolint
             source = iconv(.data[["source"]], "UTF-8", "UTF-8", sub = ""),
             source = gsub("\\.", "", .data[["source"]]))
  }
  data <- data %>%
    mutate(quintile = replace_na(.data[["quintile"]], 0)) %>%
    gather("variable", "value",
           -any_of(c("region", "period", "demographic", "quintile", "source"))) %>%
    filter(!is.na(.data[["value"]]))

  # filter specific variable
  if (!is.null(variable)) {
    if (variable %in% unique(data[["variable"]])) {
      data <- data %>%
        filter(.data[["variable"]] == !!variable)
    } else {
      stop(paste("Variable unavailable. Valid variables are: NULL,",
                 paste(paste0("'", unique(data[["variable"]]), "'"),
                       collapse = ", ")))
    }
  }

  # take mean of duplicates to remove them
  data <- data %>%
    group_by(across(-"value")) %>%
    summarise(value = mean(.data[["value"]]), .groups = "drop") %>%
    mutate(source = gsub("[^A-Za-z0-9 ]", "", .data[["source"]])) %>% #nolint
    select("region", "period", "variable", "demographic", "quintile", "source", "value") %>%
    as.magpie()

  return(data)
}
