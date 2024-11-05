#' Get Population Density
#'
#' @returns magpie object
#'
#' @author Hagen Tockhorn
#'
#' @importFrom madrat calcOutput
#' @importFrom dplyr select mutate rename
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie


calcDensity <- function() {

  # READ-IN DATA----------------------------------------------------------------

  pop <- calcOutput("Population", scenario = "SSP2", aggregate = FALSE) %>%
    as.quitte() %>%
    mutate(variable = gsub("pop_SSP2", "population", .data[["variable"]], fixed = TRUE))

  surf <- calcOutput("Surface", aggregate = FALSE) %>%
    as.quitte()


  # PARAMETERS------------------------------------------------------------------

  # Conversion from 1-e6 --> 1e0
  million2unit <- 1e6


  # PROCESS DATA----------------------------------------------------------------

  # Clean DFs
  pop <- pop %>%
    select(-"model", -"scenario", -"unit", -"variable") %>%
    mutate(value = .data[["value"]] * million2unit) %>%
    rename(population = "value")

  surf <- surf %>%
    select(-"model", -"scenario", -"unit", -"variable", -"period") %>%
    rename(surface = "value")

  # Calculate Density
  dens <- left_join(pop, surf, by = "region") %>%
    mutate(value = .data[["population"]] / (.data[["surface"]] * 1e3)) %>%
    select(-"population", -"surface")


  # Fill inf values with global density average
  avgDens <- mean(dens[!is.infinite(dens$value), ]$value)

  dens <- dens %>%
    mutate(value = ifelse(is.infinite(.data[["value"]]),
                          avgDens,
                          .data[["value"]]))



  # OUTPUT----------------------------------------------------------------------

  data <- dens %>%
    as.data.frame() %>%
    as.quitte() %>%
    as.magpie()

  return(list(x = data,
              weight = NULL,
              unit = "m^-2",
              description = "Population Density in m^-2"))
}
