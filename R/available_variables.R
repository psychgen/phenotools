#' Available variables
#'
#' \code{available_variables} returns a dataframe with names and basic info
#' of all variables available for use via phenotools
#'
#' Detailed description...
#'
#' @param source Currently any combination of "moba", "kuhr", "npr
#'
#' @return a data.frame with variable names and basic info
#'
#' @export
#' @importFrom dplyr "%>%"


available_variables <- function(source=c("moba","npr","kuhr"))
{
  all_avail <- data.frame()
  if(any(source %in%"moba")){
    temp_moba <- moba %>% dplyr::select(measure,subscale,questionnaire,var_name) %>%
      dplyr::distinct() %>%
      dplyr::mutate(source="moba")
    all_avail <- rbind(all_avail,temp_moba)
  }
  if(any(source %in%"npr")){
    temp_npr <- npr  %>%
      dplyr::mutate(var_name=ifelse(!is.na(level3), level3,
                                    ifelse(!is.na(level2),level2,chapter))) %>%
      dplyr::distinct() %>%
      dplyr::mutate(source="npr",
                    measure = paste0("ICD10 code for: ", descriptor),
                    subscale = NA,
                    questionnaire=NA) %>%
      dplyr::select(measure,subscale,questionnaire,var_name, source)
    all_avail <- rbind(all_avail,temp_npr)
  }
  if(any(source %in%"kuhr")){
        temp_kuhr<- kuhr  %>%
      dplyr::rename(var_name = Code) %>%
      dplyr::mutate(source="kuhr",
                    measure = paste0("ICPC code for: ", preferred),
                    subscale = NA,
                    questionnaire=NA)%>%
      dplyr::select(measure,subscale,questionnaire,var_name, source)
    all_avail <- rbind(all_avail,temp_kuhr)
  }


  message("Values from column:var_name are valid inputs for curate_dataset()")
  return(all_avail)
}

