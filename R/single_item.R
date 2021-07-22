#' Single item variable creation helper function
#'
#' \code{single_item} is a helper function
#' called by \code{curate_dataset} to return single item variables
#'
#' Detailed description...
#'
#' @param moba_other_vars Df made in \code{curate_dataset}
#' @param moba_other_vars_long Long form of Df made in \code{curate_dataset}
#' @param moba_data Df made in \code{curate_dataset}
#' @export
#' @importFrom dplyr "%>%"

single_item <- function(moba_other_vars,
                     moba_other_vars_long,
                     moba_data){


  si_vars <-moba_other_vars_long %>%
    dplyr::filter(measure=="single_item") %>%
    dplyr::select(var_name,item_no,item_name)

  moba_si_data <-
    suppressMessages(
      suppressWarnings(
        moba_data %>%
          dplyr::select(preg_id,m_id,f_id,BARN_NR,birth_yr,
                        si_vars$item_name) %>%
          dplyr::rename_at(dplyr::vars(-preg_id:-birth_yr), function(x){paste0(x,"_raw")} )))



  message("\nRetrieval of single item variables is complete. These will be returned
raw in both the 'scales' and 'items' datasets if out_format=\"list\", or once in the
single output dataframe if out_format=\"merged_df\".")
  return(moba_si_data)
}
