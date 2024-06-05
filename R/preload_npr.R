
#' Pre-load NPR data
#'
#' \code{preload_nor} returns a dataframe with the full NPR dataset, useful if
#' you want to run curate_dataset multiple times for different NPR variables
#' without waiting for the NPR load each time; use in conjunction with the
#' 'npr_full' option in curate_npr (passed on by curate_dataset).
#'
#'
#' Detailed description...
#'
#' @param npr_data_root_dir Where is the raw NPR phenotypic data? (default
#' is for p471)
#' @param npr_filename What is the name of the NPR file? (default
#' is for p471)

#' @export
#' @importFrom dplyr "%>%"
#' @importFrom lubridate "%--%"


preload_npr <- function(npr_data_root_dir="//ess01/P471/data/durable/data/NPR/processed/",
                        npr_filename="npr2024.sav")
{

  message(
    "\n\nReading in NPR data file (this is a large file so expect a wait of >2 mins)...")

  npr_full <- haven::read_spss(paste0(npr_data_root_dir,npr_filename)) %>%
    dplyr::mutate_at(dplyr::vars(tidyr::starts_with("tilst|NCMP|NCSP")), list(~stringr::str_remove_all(.,"[[:punct:]]"),
                                                                              ~stringr::str_trim(.,"both"),
                                                                              ~stringr::str_replace_all(., stringr::fixed(" "), ""))) %>% #Remove non alphanumeric values in all tilst- variables
    dplyr::mutate_at(dplyr::vars(tidyr::starts_with("tilst")), list(~stringr::str_sub(.,end=4))) %>%
    dplyr::mutate_if(is.character, dplyr::na_if, "") %>%
    dplyr::mutate_at(dplyr::vars(dplyr::matches("Dato")), as.Date.character )%>%
    dplyr::rename("LNr"= dplyr::matches("lnr|lopenr"),
                  "omsorgsniva3" = dplyr::matches("omsorgsniva$"),
                  "henvTypeVurd" = dplyr::matches("henvType$")) %>%
    dplyr::mutate(NCMP_x = NA, # Dummy cols in case these code cols are not included
                  NCSP_x = NA) # Dummy cols in case these code cols are not included

  message(
"\nNPR pre-load complete. You can now run curate_dataset with the object in which
this NPR data is held listed under the 'npr_full' argument (or 'npr_preprocessed'
if you have preloaded pre-processed data - see ?curate_npr for requirements).")

  return(npr_full)
}
