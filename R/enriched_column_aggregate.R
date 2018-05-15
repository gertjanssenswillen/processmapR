#' @title Aggregate map profile
#' @description Function to create a aggregate profile for a process map using the values in one of the collumns of the eventlog.
#' @param FUN A summary function to be called on the process time of a specific activity, e.g. mean, median, min, max
#' @param columnNameIn The name of the column to be aggregated
#' @param columnNameOut The name of the aggregated column
#' @param edgeOperation The operation to calculate a single value from the from and the to column in an edge
#' @export enriched_column_aggregate




enriched_column_aggregate <- function(FUN = mean, columnNameIn = NULL, columnNameOut = NULL, edgeOperation = c("mean", "min", "max", "minus", "plus","from","to")) {
    if (is.null(columnNameIn)) 
        stop("No columnNameIn specified")
    else if (is.null(columnNameOut)) 
        stop("No columnNameOut specified")
    else{
        attr(FUN, "columnNameIn") <- columnNameIn
        attr(FUN, "columnNameOut") <- columnNameOut
        attr(FUN, "perspective") <- "columnAgregate"
        attr(FUN, "edgeOperation") <- edgeOperation
    }
	return(FUN)
}
