#' Historical Final and Useful Energy Demand
#'
#' Historic Final Energy (FE) Demand and FE-EU-Efficiencies are taken to
#' compute Useful Energy (UE) Demand.
#'
#' @param endOfHistory Last historic time period
#'
#' @author Hagen Tockhorn
#'
#' @return data.frame with final and useful energy demand
#'
#' @export


calcFEUE <- function(endOfHistory = 2020) {

  # FUNCTIONS ------------------------------------------------------------------

  # Aggregate to Enduse-Level
  sumDF <- function(df, variables, newname) {
    enduseSum <- df %>%
      filter(.data[["enduse"]] %in% variables) %>%
      group_by(across(-any_of(c("value", "enduse")))) %>%
      summarise(value = sum(.data[["value"]], na.rm = TRUE), .groups = "drop") %>%
      mutate(enduse = newname) %>%
      ungroup() %>%
      select(all_of(colnames(df)))
    df %>%
      filter(!(.data[["enduse"]] %in% variables)) %>%
      rbind(enduseSum)
  }


  # READ-IN DATA ---------------------------------------------------------------

  fe <- calcOutput("FEbyEUEC", aggregate = FALSE, endOfHistory = endOfHistory) %>%
    as.quitte()


  efficiencies <- calcOutput("FEUEefficiencies", aggregate = FALSE, endOfHistory = endOfHistory) %>%
    as.quitte()


  # PROCESS DATA ---------------------------------------------------------------

  # cut datasets
  fe <- fe %>%
    select("region", "period", "enduse", "carrier", "unit", "value")

  efficiencies <- efficiencies %>%
    select("region", "period", "enduse", "carrier", "value") %>%
    rename("efficiency" = "value")


  # calculate useful energy
  ue <- fe %>%

    # unite enduses "appliances" and "lighting"
    sumDF(c("appliances", "lighting"), "appliances_light") %>%
    spread("unit", "value") %>%

    # calculate useful energy with conversion efficiencies
    left_join(efficiencies,
              by = c("region", "period", "enduse", "carrier")) %>%
    mutate(ue = .data[["fe"]] * .data[["efficiency"]]) %>%
    gather("unit", "value", "fe", "ue") %>%
    select(-"efficiency") %>%
    mutate(scenario = "history")


  # OUTPUT ---------------------------------------------------------------------

  ue <- ue %>%
    as.quitte() %>%
    as.magpie() %>%
    toolCountryFill()

  return(list(
    x = ue,
    weight = NULL,
    unit = "EJ",
    min = 0,
    description = "Final and Useful Energy Demand scaled with FE-UE-efficiencies"
  ))


}
