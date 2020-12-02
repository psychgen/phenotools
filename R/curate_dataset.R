#' Curate dataset
#'
#' \code{curate_dataset} is the main workhorse of the data preparation
#' component of phenotools, producing analysis-ready datasets with
#' specified variables from MoBa and other registry sources
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

    message("\nProcessing MoBa variables...")

    moba_vars <-
      suppressMessages(
        suppressWarnings(
          reqd_vars %>%
          dplyr::filter(source =="moba") %>%
            dplyr::left_join(moba)))

    ##W2W consistent items only?
    if(consistent_items == TRUE){
      moba_vars <- moba_vars %>%
        dplyr::mutate(items=ifelse(is.na(consistent),items,consistent ))
    }

    ##Create data.frame of PREG_IDs to aggregate created variables
    moba_data <-
      haven::read_spss(paste0(pheno_data_root_dir,"PDB",PDB,"_MBRN_541_v12.sav")) %>%
      dplyr::select(preg_id = dplyr::matches("PREG_ID"),BARN_NR) %>%
      dplyr::mutate(preg_id = as.integer(preg_id))
    ##Add M_ID and F_ID variables from SV info
    sv_info <-
      haven::read_spss(paste0(pheno_data_root_dir,"PDB",PDB,"_SV_INFO_v12.sav"))%>%
      dplyr::select(preg_id = dplyr::matches("PREG_ID"),
                    m_id = dplyr::matches("M_ID"),
                    f_id = dplyr::matches("F_ID"),
                    birth_yr = FAAR)%>%
      dplyr::mutate(preg_id = as.integer(preg_id))

    moba_data <-
      suppressMessages(
        suppressWarnings(
          moba_data %>%
            dplyr::left_join(sv_info)))%>%
      dplyr::mutate(preg_id=as.character(preg_id),
                    m_id=as.character(stringr::str_replace_all(m_id, stringr::fixed (" "), "")),
                    f_id=as.character(stringr::str_replace_all(f_id, stringr::fixed (" "), ""))) %>%
      dplyr::mutate_if(is.character, list(~dplyr::na_if(.,"")))

    #Get item-level datasets and combine

    for(q in unique(moba_vars$questionnaire)){

      message(paste0("\nLoading data from questionnaire ",q,", which is number ",match(q,unique(moba_vars$questionnaire))," of ", length(unique(moba_vars$questionnaire)) ))

      if(q %in% c("Q1","Q3","QF")){
        suppressMessages(qvars_temp <- haven::read_spss(paste0(pheno_data_root_dir,"PDB",PDB,"_", q,"_v12.sav")) %>%
                           dplyr::select(preg_id = dplyr::matches("PREG_ID"),
                                         unlist(strsplit(paste0(dplyr::filter(moba_vars,questionnaire == q)$items, collapse=","),","))) %>%
                           dplyr::mutate(preg_id=as.character(preg_id)))
      }
      if(q %in% c("Far2")){
        suppressMessages(qvars_temp <- haven::read_spss(paste0(pheno_data_root_dir,"PDB",PDB,"_", q,"_v12.sav")) %>%
                           dplyr::select(f_id = dplyr::matches("F_ID"),
                                         unlist(strsplit(paste0(dplyr::filter(moba_vars,questionnaire == q)$items, collapse=","),","))) %>%
                           dplyr::mutate(f_id=as.character(f_id)))
      }
      if(q %in% c("Q4_6months","Q5_18months","Q5yrs","Q6_3yrs","Q7yrs","Q8yrs")){
        suppressMessages(qvars_temp <- haven::read_spss(paste0(pheno_data_root_dir,"PDB",PDB,"_", q,"_v12.sav")) %>%
                           dplyr::select(preg_id = dplyr::matches("PREG_ID"), BARN_NR,
                                         unlist(strsplit(paste0(dplyr::filter(moba_vars,questionnaire == q)$items, collapse=","),","))) %>%
                           dplyr::mutate(preg_id=as.character(preg_id)))


      }

      moba_data <-
        suppressMessages(
          suppressWarnings(
            moba_data %>%
              dplyr::left_join(qvars_temp)))
    }



    # create a lookup table with relevant scales, items in long format
    moba_vars_long <- data.frame()
    for(v in unique(moba_vars$var_name)){
      temp_v <- moba_vars %>%
        dplyr::filter(var_name == v) %>%
        tidyr::separate(items, into=c(paste0("i",
                                             seq(1,length(unlist(strsplit(
                                               paste0(dplyr::filter(moba_vars,var_name == v)$items, collapse=","),",")) ))
        )
        ), sep = "," ) %>%
        tidyr::gather(item_no, item_name, -measure:-likert, -consistent:-notes)
      moba_vars_long <- rbind(moba_vars_long, temp_v)
    }

    #separate out scale and non-scale vars (defined by presence of helper function)

    moba_scale_vars <- moba_vars %>%
      dplyr::filter(is.na(helper))
    moba_scale_vars_long <- moba_vars_long %>%
      dplyr::filter(is.na(helper))
    moba_other_vars <- moba_vars %>%
      dplyr::filter(!is.na(helper))
    moba_other_vars_long <- moba_vars_long %>%
      dplyr::filter(!is.na(helper))


    # process scale vars - should create progress indicator for this as it can take a while

    if(nrow(moba_scale_vars)>0){

      moba_scales_items <-  curate_moba_scales(moba_scale_vars,
                                               moba_scale_vars_long,
                                               moba_data,
                                               return_items = return_items,
                                               consistent_items = consistent_items,
                                               completion_threshold = completion_threshold,
                                               transformations = transformations)

    }

    #process non-scale variables with calls to relevant helper functions

    moba_other_data <- moba_data %>%
        dplyr::select(preg_id:birth_yr)

    if(nrow(moba_other_vars)>0){
      message("\nProcessing non-scale MoBa vars. These are: \n\n",paste0(c(moba_other_vars$var_name), collapse="", sep="\n"))


      for(helpfun in unique(moba_other_vars$helper)){

        f <- match.fun(helpfun)
        moba_other_temp <- f(moba_other_vars,
                             moba_other_vars_long,
                             moba_data)
        moba_other_data <-
          suppressMessages(
            suppressWarnings(
              moba_other_data %>%
                dplyr::left_join(moba_other_temp)))
      }
      message("\nProcessing of non-scale MoBa variables is complete.")
    }


    message("\nCurating the final dataset(s)...")

    if(return_items==T){
      moba_data_combined <-
        suppressMessages(
          suppressWarnings(list( scales =
                                   moba_scales_items[["scales"]] %>%
                                   dplyr::left_join(moba_other_data),
                                 items =
                                   moba_scales_items$items)))
      message("\nDataset curation complete; outputting as data.frame. You requested that
scale items be returned (using return_items=TRUE), so output is a list, of which the
first element (\"scales\") is your scale-level dataset, and the second (\"items\")
is your item-level dataset, with \"_raw\" and \"_coded\" (i.e., numeric) versions of all items
for each scale.")
    }else{
      moba_data_combined <-
        suppressMessages(
          suppressWarnings(moba_scales_items %>%
                             dplyr::left_join(moba_other_data)))
message("\nDataset curation complete; outputting as data.frame.")}


  }
  #Update to merge with others sources when implmented

  return(moba_data_combined)
}

