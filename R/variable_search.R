#' Variable search
#'
#' \code{variable_search} is a helper to find variables that are available by
#' a string anywwere in the variable name or description
#'
#'
#' Detailed description..
#'
#' @param dataframe Where to look for variables -should be a data frame returned
#' by a call to \code{available_variables}
#' @param string a vector of strings to search for
#' @param where if you know the column name in which the string should be found,
#' you can supply it here; the default is to search in all columns ("anywhere")
#' @export
#' @importFrom dplyr "%>%"

variable_search <- function(dataframe, string, where="anywhere"){

  collapsed_string <- paste0(string, collapse="|") %>%  stringr::str_remove_all("\\*")

  if(where=="anywhere"){
    tmp <- dataframe %>%
      dplyr::filter( stringr::str_detect(measure, collapsed_string )|
                       stringr::str_detect(subscale, collapsed_string )|
                       stringr::str_detect(questionnaire, collapsed_string )|
                       stringr::str_detect(var_name, collapsed_string )|
                       stringr::str_detect(source, collapsed_string ))
  }else{
    tmp <- dataframe %>%
      dplyr::filter( stringr::str_detect(where, collapsed_string ))
  }

  return(tmp)

}



