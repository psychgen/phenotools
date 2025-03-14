#' Pivot curated -kuhr to long format to work with multiple occasions per individual
#'
#' \code{pivot_curated_kuhr}  can be used on a dataframe containing kuhr variables
#' made by \code{curate_kuhr} ( either directly or, more commonly via \code{curate_kuhr})
#' to pivot the all_dx variables back to long format - i.e., taking the data back
#' to one row per healthcare contact
#'
#'
#' Detailed description...
#'
#' @param curated_kuhr a dataframe produced by curate_kuhr/curate_dataset
#' with some all_dx variables in it
#' @param large_file do you have the RAM available to allow for a large file
#' (more than two sets of diagnosis/grouping variables) to be pivoted? Defaults
#' to FALSE (see Details below for suggestions if not)

#' @export
#' @importFrom dplyr "%>%"



pivot_curated_kuhr <- function(curated_kuhr,
                              large_file=FALSE)
{

  message("\nChecking input dataset...")

  if(!is.data.frame(curated_kuhr)){
    stop("Your input is not a data.frame. If curate_dataset gave you a list as
output instead, first extract the $kuhr element as input for this function, or
alternatively re-run with 'out_format = \"merged_df\"'.")
  }

  if(!any(stringr::str_detect(names(curated_kuhr),"all_dx") )){
    stop("There do not seem to be any pivot-able kuhr variables in your dataset. These
have all_dx in the name, and are usually the result of a curate_dataset command
that incorporates some (groups of) ICD-10 codes in the 'variables_required' field.
Check your input dataset.")
  }

  if(sum(stringr::str_count(names(curated_kuhr),"all_dx") )>16 & large_file==FALSE){
    stop("You have kuhr variables for more than two diagnoses/groupings in your input
dataset. Pivoting this dataset to long-format will be RAM intensive. It is
recommended that you separate your dataset and apply this function to each set
of variables corresponding to an kuhr diagnosis or grouping in turn. Alternatively,
if you think you have sufficient RAM available, re-run with 'large_file=TRUE'.")

  }

  message("\nPivoting to long format...")
  curated_kuhr_tmp <- curated_kuhr %>%
    dplyr::select(preg_id, BARN_NR, m_id, f_id, birth_yr, dx_recipient,
                  dplyr::matches("all_dx")&dplyr::matches("_kuhr") ) %>%
    tidyr::gather("var","value", -preg_id:-dx_recipient) %>%
    dplyr::mutate(n_occs = stringr::str_count(value,pattern = "; ")+1) %>%
    tidyr::separate(var, into = c("info","dx_group"), sep = "_all_dx_") %>%
    dplyr::group_split(n_occs)

  curated_kuhr_tmp_pivoted <-purrr::map(curated_kuhr_tmp, function(x){
    if(any(is.na(x$n_occs))){
      tmp <- x %>%
        dplyr::mutate(hc_contact = NA) %>%
        dplyr::select(preg_id:dx_recipient, dx_group, hc_contact, info, value)
    }else{
      tmp <- x %>%
        tidyr::separate(value, into= paste0("hc_contact_",seq(1:unique(x$n_occs))), sep = "; " ) %>%
        tidyr::gather("hc_contact","value",-preg_id:-dx_group, -n_occs )%>%
        dplyr::select(preg_id:dx_recipient, dx_group, hc_contact, info, value)
    }
    return(tmp)
  })

  curated_kuhr_pivoted <- curated_kuhr_tmp_pivoted %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(preg_id,BARN_NR,dx_group,hc_contact)

  return(curated_kuhr_pivoted)

}





