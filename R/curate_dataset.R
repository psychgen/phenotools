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
      dplyr::filter(source =="moba") %>%
      dplyr::left_join(moba)

    ##Create data.frame of PREG_IDs to aggregate created variables
    moba_data <-
      suppressMessages(
        suppressWarnings(
          haven::read_spss(paste0(pheno_data_root_dir,"PDB",PDB,"_MBRN_541_v12.sav")) %>%
            dplyr::select(preg_id = dplyr::matches("PREG_ID"),BARN_NR))) %>%
      mutate(preg_id = as.integer(preg_id))
    ##Add M_ID and F_ID variables from SV info
    sv_info <- read.delim("N:/data/durable/data/MoBaPhenoData/PDB2306_MoBa_v12/Datfiles/PDB2306_SV_INFO_v12.dat")
    colnames(sv_info) <- c("preg_id", "m_id","f_id","birth_yr")

    moba_data <- moba_data %>%
      dplyr::left_join(sv_info)%>%
      dplyr::mutate(preg_id=as.character(preg_id),
                    m_id=as.character(m_id),
                    f_id=as.character(f_id))

    #Get item-level datasets and combine

    for(q in unique(moba_vars$questionnaire)){

      message(paste0("\nLoading data from questionnaire ",match(q,unique(moba_vars$questionnaire))," of ", length(unique(moba_vars$questionnaire)) ))

      if(q %in% c("Q1","Q3","QF")){
        qvars_temp <- haven::read_spss(paste0(pheno_data_root_dir,"PDB",PDB,"_", q,"_v12.sav")) %>%
          dplyr::select(preg_id = dplyr::matches("PREG_ID"),
                        unlist(strsplit(paste0(dplyr::filter(moba_vars,questionnaire == q)$items, collapse=","),","))) %>%
          dplyr::mutate(preg_id=as.character(preg_id))
      }
      if(q %in% c("Far2")){
        qvars_temp <- haven::read_spss(paste0(pheno_data_root_dir,"PDB",PDB,"_", q,"_v12.sav")) %>%
          dplyr::select(f_id = dplyr::matches("F_ID"),
                        unlist(strsplit(paste0(dplyr::filter(moba_vars,questionnaire == q)$items, collapse=","),","))) %>%
          dplyr::mutate(f_id=as.character(f_id))
      }
      if(q %in% c("Q4_6months","Q5_18months","Q5yrs","Q6_3yrs","Q7yrs","Q8yrs")){
        qvars_temp <- haven::read_spss(paste0(pheno_data_root_dir,"PDB",PDB,"_", q,"_v12.sav")) %>%
          dplyr::select(preg_id = dplyr::matches("PREG_ID"), BARN_NR,
                        unlist(strsplit(paste0(dplyr::filter(moba_vars,questionnaire == q)$items, collapse=","),","))) %>%
          dplyr::mutate(preg_id=as.character(preg_id))


      }

      suppressWarnings(moba_data <- moba_data %>%
        dplyr::left_join(qvars_temp))
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

    # process scale vars - should create progress indicator for this as it can take a while (the below is a v rough approx)
    if(nrow(moba_scale_vars)>0){
      message(paste0("\nProcessing MoBa scale variables. These are: \n\n",paste0(c(moba_scale_vars$var_name), collapse="", sep="\n"),
                     "\nExpect a wait of up to:\n\n",round(1.191681*ncol(moba_data),1), " seconds (",
                     round((1.191681*ncol(moba_data))/60,1)," mins)\n
for your requested scales..." ))

      moba_scale_data <- moba_data %>%
        dplyr::select(preg_id,m_id,f_id,BARN_NR,birth_yr,unique(moba_scale_vars_long$item_name)) %>%
        dplyr::mutate_at(dplyr::vars(-preg_id:-birth_yr), list(~haven::as_factor(.) )) %>%
        tidyr::gather(item_name, val, -preg_id:-birth_yr) %>%
        dplyr::left_join(moba_scale_vars_long %>%
                           dplyr::select(item_name, var_name, tidyselect::matches("response") )) %>%
        dplyr::mutate(num_val = dplyr::case_when(val == response0 ~ 0,
                                                 val == response1 ~ 1,
                                                 val == response2 ~ 2,
                                                 val == response3 ~ 3,
                                                 val == response4 ~ 4) ) %>%  ### Should pre-emptively add in more response levels
        dplyr::group_by(preg_id,BARN_NR,var_name) %>%
        dplyr::summarise(score = ifelse(sum(!is.na(num_val))>=(completion_threshold*n()),
                                        round(mean(num_val, na.rm=T)*n(),0),
                                        NA)) %>%
        tidyr::spread(var_name, score)
    }
    if(nrow(moba_other_vars)>0){
      message("\nProcessing other MoBa vars. These are: \n\n",paste0(c(moba_other_vars$var_name), collapse="", sep="\n"))
    }
  }



  return(moba_data)
}

