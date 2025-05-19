#' Quickly query the items in a scale, and how phenotools combines them
#'
#' \code{query_scale_items} is a helper to find items for a given phentools scale,
#' their raw codings, and how phenotools combines them to make the scale
#'
#' Detailed description..
#'
#' @param scales a vector of phenotools variable names to search for
#' @export
#' @importFrom dplyr "%>%"

query_scale_items <- function(scales){

  collapsed_string <- paste0(scales, collapse="|") %>%  stringr::str_remove_all("\\*")

    tmp <- phenotools::moba %>%
      dplyr::filter( stringr::str_detect(var_name, collapsed_string ))

    if(nrow(tmp)==0){
      stop("No valid phenotools variable names detected - check your inputs.")
    }

    for( i in 1:nrow(tmp)){
      if(is.na(tmp[i,]$helper)){
      message(paste0("\nFor scale: ", tmp[i,]$var_name, ", phenotools uses the following items: ", tmp[i,]$items, "; ", if(is.na(tmp[i,]$reversed)){ "none" } else {tmp[i,]$reversed }, " are reversed.

                     Below are the responses coded for these variables in the dataset. Note that in some cases these differ from the MoBa documentation, or are in Norwegian rather than English.

                     Phenotools combines them by converting the first response below to a numeric value of 0, the second to 1, and so on.

                     Then, a mean of all available items is computed and multplied by the number items in the scale. If a greater proportion of items are missing than the specified completion threshold in
                     the curate_dataset command (default = 0.5, see ?curate_dataset for details), the scale value will be set to NA.

                     Response set:

                     ", tmp[i,] %>%
                       dplyr::select(matches("response")) %>%
                       paste0(collapse=", ") %>%
                       stringr::str_remove_all(", NA"), if(!is.na(tmp[i,]$notes)){paste0("

                     Here are the notes for this variable:

                     ", tmp[i,]$notes) }else {""} ))
        }else {  message(paste0("\nVariable: ", tmp[i,]$var_name, " is a derived variable, not a scale. Refer to ?moba_helpers to see how it is coded. Here, if any exist, are the notes for this variable:

                             ", tmp[i,]$notes))}

    }

}



