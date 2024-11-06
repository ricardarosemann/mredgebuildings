#' Share of energy carriers or end uses in historic buildings demand
#'
#' Shares are calculated based on energy demands in households and services from
#' the IEA ETP data base.
#' Missing shares that result from missing demand data are filled with the
#' average share across all regions and periods and then normalised to sum up to
#' one again.
#' Biomass is split according to GDP per Capita (see toolSplitBiomass).
#'
#' @param subtype specifies share
#' @param feOnly specifies if shares or quantities are returned
#'
#' @returns MAgPIE object with historic shares
#'
#' @author Robin Hasse, Antoine Levesque, Hagen Tockhorn
#'
#' @importFrom magclass mbind as.magpie
#' @importFrom madrat readSource toolCountryFill
#' @importFrom quitte as.quitte revalue.levels
#' @importFrom dplyr filter %>% mutate group_by across all_of left_join
#' summarise
#' @importFrom plyr revalue
#' @importFrom rlang .data syms
#' @importFrom tidyr separate replace_na complete
#' @importFrom utils tail
#' @export

calcShareETP <- function(subtype = c("enduse", "carrier"), feOnly = FALSE) {

  # READ-IN DATA ---------------------------------------------------------------

  # Read Buildings Data
  etp <- readSource("IEA_ETP", "buildings")

  # Get GDP per Cap
  gdppop <- calcOutput("GDPPop", aggregate = FALSE) %>%
    as.quitte() %>%
    select(-"model", -"scenario", -"unit")



  # PARAMETERS -----------------------------------------------------------------

  subtype <- match.arg(subtype)

  # Rename Variable out of Convention
  shareOf <- subtype

  # Filtered Periods and Scenarios
  periods <- c("2014")
  scen <- c("RTS")

  # Convert Unit to EJ
  PJ2EJ <- 1e-3   #nolint


  # Variable Mappings
  reval <- switch(shareOf,
    enduse = toolGetMapping(name = "enduseMap_IEA-ETP.csv",
                            type = "sectoral",
                            where = "mredgebuildings") %>%
      pull("EDGE", "IEA_ETP"),

    carrier = toolGetMapping(name = "carrierMap_IEA-ETP.csv",
                             type = "sectoral",
                             where = "mredgebuildings") %>%
      pull("EDGE", "IEA_ETP")
  )



  # PROCESS DATA ---------------------------------------------------------------

  # Map Variables
  etpFilter <- etp %>%
    as.quitte() %>%
    filter(.data[["period"]] %in% periods,
           .data[["scenario"]] %in% scen) %>%
    filter(.data[["variable"]] %in% names(reval),
           !is.na(.data[["value"]])) %>%
    mutate(variable = droplevels(revalue(.data[["variable"]], reval)))

  names(etpFilter)[names(etpFilter) == "variable"] <- shareOf


  # Extrapolate 'biotrad' share from 'biomod' values for carrier separation
  if (subtype == "carrier") {
    etpFilter <- etpFilter %>%
      rename(variable = "carrier") %>%
      toolSplitBiomass(gdppop, varName = "biomod") %>%
      rename(carrier = "variable")
  }

  # Correct precision errors
  eps <- 1e-5
  etpFilter <- etpFilter %>%
    mutate(value = ifelse(.data[["value"]] == 0,
                          eps,
                          .data[["value"]]))


  # If specified, return energy shares
  if (isFALSE(feOnly)) {

    # Global Shares
    shareGlobal <- etpFilter %>%
      group_by(across(all_of(shareOf))) %>%
      summarise(value = sum(.data[["value"]]), .groups = "drop") %>%
      ungroup() %>%
      toolCalcShares(shareOf) %>%
      mutate(value = replace_na(.data[["value"]], 0))


    # Local Shares
    share <- etpFilter %>%
      select(-"unit", -"model", -"scenario") %>%
      group_by(across(all_of(c("region", "period", shareOf)))) %>%
      summarise(value = sum(.data[["value"]]), .groups = "drop") %>%
      ungroup() %>%
      toolCalcShares(tail(shareOf, 1)) %>%
      mutate(value = replace_na(.data[["value"]], 0)) %>%
      complete(!!!syms(c("region", "period", shareOf))) %>%
      left_join(shareGlobal, by = shareOf) %>%
      mutate(value = ifelse(is.na(.data[["value.x"]]),
                            .data[["value.y"]],
                            .data[["value.x"]]),
             value = replace_na(.data[["value"]], 0)) %>%
      select(-"value.x", -"value.y") %>%
      toolCalcShares(tail(shareOf, 1))


    # Weights: Regional Share of FE
    regShare <- etpFilter %>%
      complete(!!!syms(c("region", "period", shareOf))) %>%
      interpolate_missing_periods(expand.values = TRUE) %>%
      group_by(across(all_of(c("region", "period", head(shareOf, -1))))) %>%
      summarise(value = sum(.data[["value"]], na.rm = TRUE), .groups = "drop") %>%
      group_by(across(all_of(c("period", head(shareOf, -1))))) %>%
      mutate(value = .data[["value"]] / sum(.data[["value"]]))
  }


  # CORRECTIONS ----------------------------------------------------------------

  if (subtype == "enduse") {

    # Taken from EDGE-B by Antoine Levesque:
    # "ETP electricity demand is twice as high (!) as in the IEA data,
    # I reduce the appliances and lighting and cooling demand shares as a result."

    if (isTRUE(feOnly)) {
      share <- etpFilter %>%
        droplevels() %>%
        quitte::factor.data.frame()
    }

    shareCorr <- share %>%
      mutate(value = ifelse(.data[["region"]] == "MEX" &
                              .data[["enduse"]] %in% c("appliances", "lighting"),
                            .data[["value"]] *  0.6,
                            .data[["value"]])) %>%
      mutate(value = ifelse(.data[["region"]] == "MEX" &
                              .data[["enduse"]] %in% c("space_cooling"),
                            .data[["value"]] *  0.6,
                            .data[["value"]])) %>%
      mutate(value = ifelse(.data[["region"]] %in% c("RUS", "IND") &
                              .data[["enduse"]] %in% c("appliances", "lighting", "space_cooling"),
                            .data[["value"]]  * 0.85,
                            .data[["value"]])) %>%
      mutate(value = ifelse(.data[["region"]] == "USA" &
                              .data[["enduse"]] %in% c("appliances", "lighting"),
                            .data[["value"]]  * 0.85,
                            .data[["value"]]))

    if (isFALSE(feOnly)) {
      # re-normalize data
      share <- shareCorr %>%
        group_by(across(all_of(c("region", "period")))) %>%
        mutate(value = proportions(.data[["value"]])) %>%
        ungroup()
    } else {
      etpFilter <- shareCorr
    }
  }



  # OUTPUT ---------------------------------------------------------------------

  # return only FE data
  if (isTRUE(feOnly)) {
    feData <- etpFilter %>%
      as.quitte() %>%
      mutate(value = .data[["value"]] * PJ2EJ,
             unit = "EJ") %>%
      select("region", "period", "unit", shareOf, "value") %>%
      as.magpie() %>%
      toolCountryFill(verbosity = 0)

    return(list(x = feData,
                unit = "EJ/yr",
                min = 0,
                description = "FE of carrier or end use in buildings demand in EJ"))
  }


  # Convert to Magpie Object
  share <- share %>%
    droplevels() %>%
    as.magpie(spatial = 1) %>%
    toolCountryFill(verbosity = 0)


  regShare <- regShare %>%
    as.magpie(spatial = 1) %>%
    collapseDim() %>%
    toolCountryFill(1, verbosity = 0)


  # Generate Output
  return(list(x = share,
              weight = regShare,
              unit = "1",
              min = 0,
              max = 1,
              description = "Share of carrier or end use in buildings demand"))
}
