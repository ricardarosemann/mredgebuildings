#' Converts Data from Primary Fuel and Useful Energy Data Base (PFUDB)
#'
#' PFU Data is disaggregated into different thermal and non-thermal Enduses
#' using FE Shares from Odyssee and IEA_ETP.
#'
#' This was adapted from EDGE function 'getPFUDB.R'.
#'
#' @returns magpie object
#'
#' @author Hagen Tockhorn, Robin Hasse
#'
#' @importFrom madrat readSource toolGetMapping readSource
#' @importFrom quitte aggregate_map as.quitte factor.data.frame revalue.levels
#' @importFrom dplyr %>% .data filter group_by summarise mutate ungroup select
#'   all_of any_of pull
#' @export

calcPFUDB <- function() {

  # FUNCTIONS ------------------------------------------------------------------

  # replace NA's only if not all values are NA
  replaceNAwithZero <- function(x) {
    if (all(is.na(x))) {
      return(NA)
    } else {
      return(replace(x, is.na(x), 0))
    }
  }



  # PARAMETERS -----------------------------------------------------------------

  # Rename Carrier Items
  carriersnames <- toolGetMapping("carrierMap_PFUDB.csv",
                                  type = "sectoral",
                                  where = "mredgebuildings") %>%
    pull("EDGE", "PFUDB")

  # Enduse-Carrier combinations which will be systematically excluded
  exclude <- toolGetMapping("excludeEnduseCarrier.csv",
                            type = "sectoral",
                            where = "mredgebuildings")

  # fridge electricity shares (see calcShares)
  fridgeShare <- rbind(data.frame(RegionCode = "USA", share  = 0.12),
                       data.frame(RegionCode = c("EUR", "OCD", "RUS", "JPN"), share = 0.17),
                       data.frame(RegionCode = c("CHN", "IND", "NCD", "AFR", "MIE", "OAS"), share = 0.3))

  # lower temporal threshold of historical data
  periodBegin <- 1990



  # READ-IN DATA ---------------------------------------------------------------

  pfu <- readSource("PFUDB") %>%
    as.quitte()


  # Relevant toolDisaggregate Input-Data

  sharesEU <- calcOutput("Shares",
                         subtype = "enduse_thermal",
                         aggregate = TRUE,
                         regionmapping = "regionmappingEUshares.csv") %>%
    as.quitte()


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



  # Mappings

  # EU Shares region mapping
  regmapping <- toolGetMapping(name  = "regionmappingEUshares.csv",
                               type  = "regional",
                               where = "mredgebuildings") %>%
    select("region", "regionAgg")

  # Enduse Mappings
  enduseMapping <- toolGetMapping(name  = "enduseMap_PFUDB.csv",
                                  type  = "sectoral",
                                  where = "mredgebuildings")

  # EDGE mapping
  regmappingEDGE <- toolGetMapping(name  = "regionmappingEDGE.csv",
                                   type  = "regional",
                                   where = "mredgebuildings")



  # PROCESS DATA ---------------------------------------------------------------

  sharesEU <- sharesEU %>%
    select("region", "period", "enduse", "value")


  pfu <- pfu %>%
    # Generalize Heat Carriers
    aggToLevel(variables = c("Heat", "Geothermal", "Solar"), level = "carrier", newname = "heat") %>%

    # Map Carrier Names and Convert Units
    revalue.levels(carrier = carriersnames) %>%
    factor.data.frame() %>%
    mutate(value = replace_na(.data[["value"]], 0)) %>%
    select("region", "period", "unit", "carrier", "enduse", "value")  %>%
    semi_join(sharesEU, by = c("period"))



  ## Disaggregate into Thermal and Non-Thermal Part ====

  # Aggregate uses from PFU to appliances_light and keep the df as Non-thermal
  pfuNonTherm <- pfu %>%
    filter(.data[["enduse"]] != "Low-T heat") %>%
    aggregate_map(mapping = enduseMapping,
                  by = "enduse",
                  variable = "enduse",
                  forceAggregation = TRUE)



  ## Prepare toolDisaggregate Input ====

  # mix already existing disaggregated data for share estimation
  feDisagg <- feOdyssee %>%
    left_join(feIEAEEI,
              by = c("region", "period", "carrier", "enduse")) %>%
    mutate(value = ifelse(is.na(.data[["value.x"]]),
                          .data[["value.y"]],
                          .data[["value.x"]])) %>%
    toolAddThermal(regmappingEDGE, fridgeShare) %>%
    select("region", "period", "carrier", "enduse", "value")


  # calculate shares for already disaggregated data
  sharesReplace <- feDisagg %>%
    group_by(across(all_of(c("region", "period")))) %>%
    mutate(value = replaceNAwithZero(.data[["value"]])) %>%
    ungroup() %>%
    group_by(across(all_of(c("region", "period", "carrier")))) %>%
    mutate(value = proportions(.data[["value"]])) %>%
    ungroup()


  # Extract regions with existing disaggregated FE shares
  replaceRegs <- sharesReplace %>%
    filter(!is.na(.data[["value"]])) %>%
    pull("region") %>%
    unique()


  # exclude specific carrier/enduse combinations
  sharesReplace <- rbind(
    sharesReplace %>%
      semi_join(exclude, by = c("enduse", "carrier")) %>%
      mutate(value = 0),
    sharesReplace %>%
      anti_join(exclude, by = c("enduse", "carrier"))
  )


  # data excluded from disaggregation
  pfuThermExclude <- pfu %>%
    # exclude regions with existing disaggregated shares
    filter(.data[["enduse"]] == "Low-T heat",
           .data[["region"]] %in% replaceRegs) %>%
    select(-"enduse") %>%

    # disaggregate with existing shares
    left_join(sharesReplace %>%
                select("region", "period", "carrier", "enduse", "value") %>%
                rename(share = "value"),
              by = c("region", "period", "carrier"),
              relationship = "many-to-many") %>%
    mutate(value = .data[["value"]] * .data[["share"]],
           value = replace_na(.data[["value"]], 0)) %>%
    select(-"share")


  # Disaggregate Low-T Heat into different enduses
  pfuThermFE <- pfu %>%
    filter(.data[["enduse"]] == "Low-T heat",
           !(.data[["region"]] %in% replaceRegs),
           .data[["unit"]] == "fe") %>%
    select(-"enduse") %>%
    toolDisaggregate(enduseShares  = sharesEU,
                     outliers      = c("IND", "CHN", "ZAF"),
                     exclude       = exclude,
                     dataDisagg    = feDisagg,
                     regionMapping = regmapping) %>%
    select(colnames(pfuNonTherm))


  # Use carrier-enduse distribution to apply on useful energy
  shares <- pfuThermFE %>%
    group_by(across(all_of(c("region", "period", "carrier")))) %>%
    mutate(share = proportions(.data[["value"]])) %>%
    select(-"value", -"unit")


  # disaggregate useful energy with calculated shares
  pfuTherm <- pfu %>%
    filter(.data[["enduse"]] == "Low-T heat",
           .data[["unit"]]   == "ue",
           !(.data[["region"]] %in% replaceRegs)) %>%
    select(-"enduse") %>%
    left_join(shares, by = c("region", "period", "carrier")) %>%
    mutate(value = .data[["value"]] * .data[["share"]]) %>%
    select(-"share") %>%
    rbind(pfuThermFE)


  # Join Non-Thermal and Thermal Part
  pfuRes <- rbind(pfuTherm, pfuThermExclude, pfuNonTherm)


  # Include "refrigerators" in "appliances_light"
  pfuRes <- rbind(pfuRes %>%
                    filter(!(.data[["enduse"]] %in% enduseMapping[4:5, "enduse"])),
                  pfuRes %>%
                    aggregate_map(mapping = enduseMapping[4:5, ],
                                  by = "enduse",
                                  variable = "enduse",
                                  forceAggregation = TRUE))

  # Select data above lower history boundary
  pfuRes <- pfuRes %>%
    filter(.data[["period"]] >= periodBegin)



  # OUTPUT ---------------------------------------------------------------------

  result <- pfuRes %>%
    as.quitte() %>%
    as.magpie()

  return(list(x = result,
              weight = NULL,
              unit = "EJ",
              min = 0,
              description = "Primary Fuel and Useful Energy Data Base"))
}
