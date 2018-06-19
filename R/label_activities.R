#' @title label_activities
#'
#'
#' @description A function for filling the activities in a process map DiagrammeR object
#' @param diagrammerProcessMap The diagrammer object for which we will add values to the attributes
#' @param columns Defines in a vector the names of the columns containing the captions and/or metrics to be used
#'
#' @examples
#' \dontrun{
#' library(eventdataR)
#' data(patients)
#' map <- process_map(patients, render = F)
#'
#' }
#' @export label_activities

label_activities <- function( diagrammerProcessMap,
                              columns =c("activity_name"),
                              heightfactor = 3,
                              rounding = 2
){
    columnsSize <- length(columns)
    if (columnsSize > 1) {
        diagrammerProcessMap$nodes_df$tempVarGjdasflx <-  apply( smartRounderDF(get_node_df(diagrammerProcessMap)[ , columns ],rounding) , 1 , paste , collapse = "\n\r" )
        diagrammerProcessMap$nodes_df$height = columnsSize/heightfactor
    }
    else if (columnsSize == 1) {
        diagrammerProcessMap$nodes_df$tempVarGjdasflx <-  smartRounderV(get_node_df(diagrammerProcessMap)[, columns[1]],rounding)
        diagrammerProcessMap$nodes_df$height = 1/heightfactor
    }
    else { #no columns
        diagrammerProcessMap$nodes_df$tempVarGjdasflx <- ""
        diagrammerProcessMap$nodes_df$height = 1/heightfactor
    }
    
    diagrammerProcessMap %>%
        mutate_node_attrs(label = tempVarGjdasflx)
}

smartRounderDF <- function(r,s = 2){
    as.data.frame(lapply(r,smartRounderV, s = s))
}


smartRounderV <- function(r,s=2){
    if (is.numeric(r)) sapply(r,smartRounder,s,simplify = TRUE)
    else r
}

smartRounder <- function(r,s = 2){
    if (r == 0) return(r)
    sizeR <- round(log10(r))
    if (sizeR > s*2) return(formatC(r, format = "e", digits = 2))
    if (sizeR > s) return(round(r))
    if (sizeR > -s)  return(round(r* 10^(s-sizeR-1))/10^(s-sizeR-1))
    formatC(r, format = "e", digits = 2)
}

