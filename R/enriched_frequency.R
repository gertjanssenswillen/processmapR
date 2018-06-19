#' @title Frequency map profile
#' @description Function to create a frequency profile for a process map.
#' @param value The type of frequency value to be used: absolute or relative.
#' @export enriched_frequency


enriched_frequency <- function(value = c("absolute", "relative"),columnName = NULL) {
	value <- match.arg(value)
	attr(value, "perspective") <- "frequency"
	if (is.null(columnName)) { 
	    attr(value, "columnName") <- paste0("frequency_",value)
	} else {
	    attr(value, "columnName") <- columnName
	}
	return(value)
}

