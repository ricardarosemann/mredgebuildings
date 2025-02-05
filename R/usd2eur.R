#' Exchange rate between USD and EUR
#'
#' @param year integer, reference year
#' @returns MER EUR in USD in given year
#'
#' @author Robin Hasse
#'
usd2eur <- function(year = 2020) {
  # A quick way to return the MER [EUR per USD] in a given year.
  GDPuc::toolConvertSingle(1, "DEU", year, unit_in = "current US$MER", unit_out = "current LCU")
}
