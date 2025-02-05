#' fullEDGEBUILDINGS
#'
#' Function that produces the complete ISO data set required for the
#' EDGE - Buildings model.
#'
#' @param rev data revision which should be used as input (positive numeric).
#'
#' @author Antoine Levesque, Robin Hasse, Hagen Tockhorn
#'
#' @seealso
#' \code{\link{readSource}},\code{\link{getCalculations}},\code{\link{calcOutput}}
#' @examples
#' \dontrun{
#' fullEDGEBUILDINGS()
#' }
#'
#' @importFrom madrat calcOutput
#' @export

fullEDGEBUILDINGS <- function(rev = 0) {

  # socio-economic data --------------------------------------------------------
  scenarios <- c("SSPs", "SDPs")
  calcOutput("Population",  scenario = scenarios, file = "f_pop.cs4r")
  calcOutput("GDP",         scenario = scenarios, file = "f_gdp.cs4r", average2020 = FALSE, unit = "constant 2005 Int$PPP")
  calcOutput("Surface",                           file = "f_surface.cs4r")
  calcOutput("Urban",       scenario = scenarios, file = "f_urban.cs4r")

  # climate data ---------------------------------------------------------------
  calcOutput("HDDCDD", fromSource = TRUE, file = "f_hddcdd.cs4r")

  # energy ---------------------------------------------------------------------
  calcOutput("IOEdgeBuildings", subtype = "output_EDGE_buildings", file = "f_edge_buildings.cs4r")
  calcOutput("IOEdgeBuildings", subtype = "output_EDGE",           file = "f_edge_stationary.cs4r")
  calcOutput("FloorspacePast",                                     file = "f_floorspace.cs4r")
  calcOutput("TCEP", subtype = "floorspace",    aggregate = FALSE, file = "f_floorspace_tcep.cs4r")
  calcOutput("FEUE",                                               file = "f_feue.cs4r")
  calcOutput("FEUEefficiencies",                                   file = "f_feue_efficiencies.cs4r")
  calcOutput("EfficiencyRegression",            aggregate = FALSE, file = "f_feue_efficiencyPars.cs4r")
  calcOutput("UValueSource", subtype = "ETSAP", aggregate = FALSE, file = "f_uvalues_etsap.cs4r")
  calcOutput("UValueSource", subtype = "EUBuildingsObservatory", aggregate = FALSE,  file = "f_uvalues_rescom.cs4r")

}
