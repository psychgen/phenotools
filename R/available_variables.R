#' Available variables
#'
#' \code{available_variables} returns a dataframe with names and basic info
#' of all variables theoretically available for retrieval via phenotools (actual
#' availability depends upon the source datafiles in your project - use the
#' \code{project_specific} option and/or speak to your project administrator
#' to find out what should be available to you).
#'
#'
#' @param source Currently any combination of "moba", "kuhr", "npr
#' @param include_unprocessed Should raw MoBa questionnaire items that can be
#' retrieved be included in the output? Defaults to TRUE
#' @param project_specific Should output be filtered based on what is actually
#' available in the project in which you are working? Defaults to NULL - i.e.,
#' output is all variables theoretically retrievable by phenotools. Alternatives
#' are: "p471" (uses an internally-stored list of variables available in the
#' p471 project in which phenotools is developed) or a filepath to a .csv file
#' with (at least) a "var_name" column listing all variables and diagnostic
#' codes (e.g., from NPR or KUHR) available in the project in which you are
#' working.
#'
#' @return a data.frame with variable names and basic info
#'
#' @export
#' @importFrom dplyr "%>%"


available_variables <- function(source=c("moba","npr","kuhr"),
                                include_unprocessed = T,
                                project_specific = NULL)
{
  all_avail <- data.frame()
  if(any(source %in%"moba")){
    temp_moba <- phenotools::moba %>% dplyr::select(measure,subscale,questionnaire,var_name) %>%
      dplyr::distinct() %>%
      dplyr::mutate(source="moba")
    if(include_unprocessed==T){
      temp_moba_items <- purrr::map(phenotools::moba_varnames,  dplyr::as_tibble) %>%
        dplyr::bind_rows(.id="questionnaire") %>%
        dplyr::select(questionnaire, "var_name"=value) %>%
        dplyr::mutate(source="moba") %>%
        dplyr::filter(!stringr::str_detect(var_name,"ID"))
      all_avail <- dplyr::bind_rows(all_avail,temp_moba,temp_moba_items)
    }else {
      all_avail <- dplyr::bind_rows(all_avail,temp_moba)
    }
  }
  if(any(source %in%"npr")){
    temp_npr <- phenotools::npr  %>%
      dplyr::mutate(var_name=ifelse(!is.na(level4), level4,
                                    ifelse(!is.na(level3),level3,
                                           ifelse(!is.na(level2),level2,chapter))))  %>%
      dplyr::distinct() %>%
      dplyr::mutate(source="npr",
                    measure = paste0("ICD10 code for: ", descriptor),
                    subscale = NA,
                    questionnaire=NA) %>%
      dplyr::select(measure,subscale,questionnaire,var_name, source) %>%
      dplyr::mutate(measure=stringr::str_replace_all(measure,"ICD10 code for: NA", "ICD10 code for: unknown/not used"))
    all_avail <- rbind(all_avail,temp_npr)
  }
  if(any(source %in%"kuhr")){
    temp_kuhr<- phenotools::kuhr  %>%
      dplyr::rename(var_name = Code) %>%
      dplyr::mutate(source="kuhr",
                    measure = paste0("ICPC code for: ", preferred),
                    subscale = NA,
                    questionnaire=NA)%>%
      dplyr::select(measure,subscale,questionnaire,var_name, source)
    all_avail <- rbind(all_avail,temp_kuhr)
  }


  if(!is.null(project_specific)){

    if(project_specific=="p471"){
      message("Filtering output to variables available in p471...")

      all_avail <- all_avail %>%
        dplyr::inner_join(phenotools::p471)


    }else if(stringr::str_detect(project_specific,".csv")){
      if(!file.exists(project_specific)){
        warning("\nThe filepath you provided does not lead to a .csv file with a column
              called var_name identifying variables available in the current TSD
              project. The output will NOT be filtered by project specific availability.)\n")
      }else {
        tmp = readr::read_csv(project_specific)

        if(!stringr::str_detect(colnames(tmp),"var_name")){
          warning("\nThe file you provided does not have a column
              called var_name identifying variables available in the current TSD
              project. The output will NOT be filtered by project specific availability.)\n")
        } else {
          all_avail <- all_avail %>%
            dplyr::right_join(tmp %>% dplyr::select(var_names))

        }
      }


    }else if(project_specific!="p471"|stringr::str_detect(project_specific,".csv")){
      warning("\nThe filepath you provided does not lead to a .csv file with a column
              called var_name identifying variables available in the current TSD
              project. The output will NOT be filtered by project specific availability.)\n")
    }

  }
  message("\nValues from column:var_name are valid inputs for curate_dataset()")
  return(all_avail)
}

