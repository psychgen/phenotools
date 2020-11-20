#' Initialise project
#'
#' \code{initialise_project} populates a specified directory
#' with the file structure required to make full use of phenotools
#'
#' Detailed description...
#'
#' @param scales_required What scales are required? See
#' \code{available_variables} for a valid inputs
#' @param pheno_data_root_dir Where is the raw MoBa phenotypic data?
#' @param PDB What is the PDB code for your TSD project?
#' @param completion_threshold What proportion of scale items need
#' to be present for a scale score to be computed?
#' @param return_items Output item-level data?
#' @param consistent_items Only use wave-to-wave consistent items
#' for scales that are measured longitudinally?
#' @param transformations Not yet implemented
#' @export
#' @importFrom dplyr "%>%"


curate_dataset <- function(scales_required="none_specified",
                           pheno_data_root_dir="N:/data/durable/data/MoBaPhenoData/PDB2306_MoBa_V12/SPSS/",
                           PDB="2306",
                           completion_threshold=0.5,
                           return_items=FALSE,
                           consistent_items=FALSE,
                           transformations=NULL){

  message("Function in development...")

}

