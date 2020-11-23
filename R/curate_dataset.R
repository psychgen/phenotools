#' Initialise project
#'
#' \code{initialise_project} populates a specified directory
#' with the file structure required to make full use of phenotools
#'
#' Detailed description...
#'
#' @param variables_required What variables are required? See
#' \code{available_variables} for a valid inputs
#' @param pheno_data_root_dir Where is the raw MoBa phenotypic data?
#' @param PDB What is the PDB code for your TSD project?
#' @param completion_threshold What proportion of scale items need
#' to be present for a scale score to be computed?
#' @param return_items Output item-level data?
#' @param consistent_items Only use wave-to-wave consistent items
#' for variables that are measured longitudinally?
#' @param transformations Not yet implemented
#' @export
#' @importFrom dplyr "%>%"


curate_dataset <- function(variables_required="none_specified",
                           pheno_data_root_dir="N:/data/durable/data/MoBaPhenoData/PDB2306_MoBa_V12/SPSS/",
                           PDB="2306",
                           completion_threshold=0.5,
                           return_items=FALSE,
                           consistent_items=FALSE,
                           transformations=NULL){

  message("Checking inputs...")

  ##Error if invalid variable is requested
  if (any(variables_required %in%c("none_specified")) )
    stop(paste0("No variable(s) selected.\n\nFor a list of valid variable names run avaliable_variables()"))
  ##Error if invalid variable is requested
  if (length(variables_required[(!variables_required %in% suppressMessages(available_variables()$var_name))])>=1 )
    stop(paste0("Invalid variable(s) selected. Invalid variables:\n\n",
                paste0(variables_required[(!variables_required %in% suppressMessages(available_variables()$var_name))], collapse="\n"),
                "\n\nFor a list of valid variable names run avaliable_variables()"))

  reqd_vars <- suppressMessages(available_variables(source = c("moba","npr","kuhr"))) %>%
    dplyr::filter(var_name %in% variables_required)

  sources <- reqd_vars %>%
    dplyr::select(source) %>% dplyr::distinct()


  #MOBA
  ###################################################################################
  if(any(sources$source == "moba")){

    moba_vars <- reqd_vars %>%
      dplyr::filter(source =="moba")

    ##Create data.frame of PREG_IDs to aggregate created variables
    moba_data <-
      suppressMessages(
        suppressWarnings(
          haven::read_spss(paste0(pheno_data_root_dir,"PDB",PDB,"_MBRN_541_v12.sav")) %>%
            dplyr::select(preg_id = dplyr::matches("PREG_ID"),BARN_NR)))

    #Get item-level datasets and combine
    if(any(unique(moba_vars$questionnaire)=="Q1")){
      q1vars <- haven::read_spss(paste0(pheno_data_root_dir,"PDB",PDB,"_Q1_v12.sav")) %>%
        dplyr::select(preg_id = dplyr::matches("PREG_ID"),
                      unlist(strsplit(paste0(dplyr::filter(moba,questionnaire %in% "Q1")$items, collapse=","),",")))
    }


  }



 return(moba_data)
}

