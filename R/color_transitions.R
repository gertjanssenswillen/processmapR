#' @title color_transitions
#'
#'
#' @description A function for coloring the transitions in a process map DiagrammeR object
#' @param diagrammerProcessMap The diagrammer object for which we will add values to the attributes
#' @param column Defines the column containing the metric to be used
#' @param colorCaption Defines the color for to be used for the caption
#' @param colorLowerbound Defines the color lower bound to be used for the coloring scale of the transitions
#' @param colorUpperbound Defines the color upper bound to be used for the coloring scale of the transitions
#' @param lowerPenSize Defines the lower pensize bound to be used for the pen size scale of the transitions
#' @param upperPenSize Defines the upper pensize bound to be used for the pen size scale of the transitions
#'
#' @examples
#' \dontrun{
#' library(eventdataR)
#' data(patients)
#' map <- process_map(patients, render = F)
#' 
#' }
#' @export color_transitions

color_transitions <- function( diagrammerProcessMap,
                         column,
                         colorCaption  = "black",
                         colorLowerbound= "gray50",
                         colorUpperbound = "gray100",
                         lowerPenSize = 0.5,
                         upperPenSize = 5){
    diagrammerProcessMap %>%
        rescale_edge_attrs(
            edge_attr_from = !!column,
            to_lower_bound = colorLowerbound,
            to_upper_bound = colorUpperbound,
            edge_attr_to = color ) %>%
        rescale_edge_attrs(
            edge_attr_from = !!column,
            to_lower_bound = colorCaption,
            to_upper_bound = colorCaption,
            edge_attr_to = fontcolor) %>%
        rescale_edge_attrs(
            edge_attr_from = !!column,
            to_lower_bound = lowerPenSize,
            to_upper_bound = upperPenSize,
            edge_attr_to = penwidth)
}