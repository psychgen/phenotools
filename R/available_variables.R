#' Available variables
#'
#' \code{available_variables} returns a dataframe with names and basic info
#' of all variables available for use via phenotools
#'
#' Detailed description...
#'
#' @param source Currently "moba", "kuhr", "npr
#' @export
#' @importFrom dplyr "%>%"


available_variables <- function(source="moba")
{
  if(any(source %in%"moba")){
    message("Values from column:var_name are valid inputs for curate_dataset()")
    temp <- moba %>% dplyr::select(measure,subscale,questionnaire,var_name) %>%
      dplyr::distinct() %>%
      dplyr::mutate(source="moba")
  }
  if(any(source %in%"npr")){
    message("source npr not yet implemented")
  }
  if(any(source %in%"kuhr")){
    message("source npr not yet implemented")
  }
  return(temp)
}

