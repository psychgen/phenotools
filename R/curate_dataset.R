#' Curate dataset
#'
#' \code{curate_dataset} is the main workhorse of the data preparation
#' component of phenotools, producing analysis-ready datasets with
#' specified variables from MoBa and other registry sources
#'
#' This function fulfills the main purpose of the phenotools package by taking
#' details of variables you want in your analytic dataset, retrieving and processing
#' the raw data needed to make those variables, and returning them to you in either a
#' dataframe or list of dataframes. For the function to work, you need to be able
#' to direct it to the local directory that is home to the raw (i.e., as received)
#' MoBa phenotypic data files >=v11 in SPSS format. Remember, datasets can only be
#' curated on the basis of variables that exist in your raw MoBa data files. If something
#' is not working, it is worth checking MoBa documentation for item codes and verifying that
#' they exist in your raw data.
#'
#' For a full introduction to the \code{curate_dataset} function, see the package
#' vignette (\code{vignette("phenotools")}).
#'
#' @param variables_required What variables are required? See
#' \code{available_variables} for a valid inputs
#' @param moba_data_root_dir Where is the raw MoBa phenotypic data?
#' @param PDB What is the PDB code for your TSD project?
#' @param moba_data_version Which version of MoBa phenotypic data are you using?
#' Defaults to 12; 11 is also a viable option for some scales
#' @param completion_threshold What proportion of scale items need
#' to be present for a scale score to be computed?
#' @param return_items Output item-level data?
#' @param consistent_items Only use wave-to-wave consistent items
#' for variables that are measured longitudinally?
#' @param transformations Not yet implemented
#' @param log filepath for code_preparation log - not yet implemented
#' @param out_format should output be a "list" or "merged_df"; defaults to "list"
#' @param override_filenames questionnaire filenames are built by combining your
#' inputs on PDB, moba_data_version, and the standard codes for each questionnaire
#' (see unique(available_variables("moba")$questionnaire) + "SV_INFO" and "MBRN")
#' to give the format: "PDB>PDB<_>code<_v>moba_data_version<.sav"). If you need to
#' override this because the filenames have been altered in your TSD project, add
#'  a vector of strings here, e.g., 'override_filenames = c("Q1 = new_Q1_filename.sav").
#'  The SV_INFO and MBRN files are accessed implicitly for every curate call, so
#'  you will need to provide filenames for these (as well as the questionnaire files
#'  from which your variables are to be sourced) if they differ from the expected format
#'  is a reminder that these sources
#' @param ... arguments to pass to internal functions, see ?curate_moba_scales
#' and ?curate_npr
#' @export
#' @importFrom dplyr "%>%"
#'
#'
#' @examples
#'
#' # Curate a scale- and item-level dataet from MoBa only, returned as a list
#'
#' mydata <- curate_dataset(variables_required=c("scl_anx_m_3yr","bmi_derived_f_q1"),return_items = T, out_format="list")
#'
#' # Curate a scale- and item-level dataet from MoBa only, returned as a merged dataframe
#'
#' mydata <- curate_dataset(variables_required=c("scl_anx_m_3yr","bmi_derived_f_q1"),return_items = T, out_format="merged_df")
#'
#' # Curate a dataset from multiple sources
#'
#' mydata <- curate_dataset(variables_required=list(moba = c("scl_anx_m_3yr","bmi_derived_f_q1"),
#' npr = random5nprcodes),
#' return_items = T, out_format="merged_df",
#' exclusions=NULL,
#' recursive=TRUE,
#' group_all=TRUE,
#' dx_groupname="ourdiagnoses",
#' dx_owner="child")
#'
#' For more examples, see the package vignette (\code{vignette("phenotools")}).


curate_dataset <- function(variables_required,
                           moba_data_root_dir="//ess01/P471/data/durable/data/MoBaPhenoData/PDB2306_MoBa_V12/SPSS/",
                           PDB="2306",
                           moba_data_version = 12,
                           completion_threshold=0.5,
                           return_items=FALSE,
                           consistent_items=FALSE,
                           transformations=NULL,
                           log=NULL,
                           out_format = "list",
                           override_filenames = NULL,
                           ...){

  ################
  #LOGGING FUNCTIONS
  ################
  if(!is.null(log)){
    #Check for logr
    if(! "logr" %in% installed.packages()[,"Package"]){
      stop("\nThe logr package is required to use phenotools' logging functionality.
Either install it or re-run with log=NULL.")
    }

    suppressMessages <- function(x){x}
   # suppressWarnings <- function(x){x}

    # Open log
    logr::log_open(log, autolog = TRUE, show_notes = TRUE)

    # Print log header
    logr::log_print("Dataset Curation Log: data preparation using the phenotools package", console=FALSE)


  }

  ################
  #INPUT CHECKING
  ################

  message("Checking inputs...")
  ##Warn if MoBa version is not 12
  if(moba_data_version!=12){
    warning("MoBa variables have been curated and QCd using v12 data. Coding differences in different versions may mean that some variables are computed incorrectly. It is recommended that you use phenotools with v12 MoBa data. If you really do need to use alternative versions, check your variables carefully, using return_items=TRUE to get raw and coded items for all scales, and checking that these appear to correspond correctly.")
  }

  ##Error no variables requested
  if (!exists("variables_required") ){
    stop(paste0("No variable(s) specified in 'variables_required'.\n\nFor a list of valid variable names run avaliable_variables()"))
  }

  ## Function to find MoBa items

  moba_item_finder <- function(var){
    res<- lapply(phenotools::moba_varnames, function(ch) grep(paste0("\\b",var,"\\b"), ch))
    res2 <- sapply(res, function(x) length(x) > 0)
    if(!any(res2)==TRUE){
      return(NA)
    }else{
      names(which(res2))
    }
  }

  ## If variables_required is not a named list, can only work with MoBa variables

  ## Process if not list


  if(!is.list(variables_required)){

        ##Pull out any "new" variables
    if (length(variables_required[(!variables_required %in% suppressMessages(available_variables("moba")$var_name))])>=1 ){

      new_vars <- variables_required[(!variables_required %in% suppressMessages(available_variables("moba")$var_name))]
      moba_new_vars <- sapply(new_vars,moba_item_finder) %>%
        tibble::enframe() %>%
        `colnames<-`(c("var_name", "questionnaire")) %>%
        dplyr::mutate(measure = ifelse(!is.na(questionnaire),"single_item",NA),
                      subscale = rep(NA,length(new_vars)),
                      source = ifelse(!is.na(questionnaire),"moba",NA))

      other_vars <- c(moba_new_vars %>%
                        dplyr::filter(is.na(source)) %>%
                        .$var_name,
                      variables_required[(variables_required %in% suppressMessages(available_variables(c("npr","kuhr"))$var_name))] ) %>%
        unique()

      if(length(other_vars)>0){
        stop(paste0("The following variables cannot be found within MoBa data: ", paste0(other_vars,collapse=","), ".

If these come from other sources, please convert your input for the variables_required argument into a named
list and re-run (see vignette(\"phenotools\") for details).

If you think this variable is available within your source MoBa datafiles and should be accessible to phenotools, please contact lauriejhannigan@gmail.com.
For a list of valid pre-processed variable names run avaliable_variables(source = \"moba\"), or check the MoBa wiki for valid item codes."))

      }

    }

    if(exists("moba_new_vars")){
      reqd_vars <- suppressMessages(available_variables(source = c("moba"))) %>%
        dplyr::filter(var_name %in% variables_required) %>%
        dplyr::bind_rows(suppressMessages(available_variables()) %>%
                           dplyr::filter(is.null(var_name)) %>%
                           dplyr::bind_rows(moba_new_vars))
    }else {
      reqd_vars <- suppressMessages(available_variables(source = c("moba"))) %>%
        dplyr::filter(var_name %in% variables_required)
    }

  }else{

    ## Process if list

    #check that list elements are named
    if(is.null(names(variables_required))){
      stop("List elements in variables_required must be named.

If you are only curating based on MoBa variables, you can re-run with the input for
variables_required as a vector, without names. Otherwise, name your list elements
according to the data source from which the variables come (see vignette(\"phenotools\") for details).")
    }
    #check that names are recognizable
    if(any(!names(variables_required) %in% suppressMessages(unique(available_variables()$source)) )){
      stop(paste0("List elements in variables_required must be named according to source.

Run unique(available_variables()$source) to see how to specify source names.

Unrecognised source names: ", names(variables_required)[!names(variables_required) %in% suppressMessages(unique(available_variables()$source))]) )
    }

    #unlist the inputs
    variables_required <- unlist(variables_required) %>%
      as.data.frame() %>%
      tibble::rownames_to_column() %>%
      `colnames<-`(c("source","var_name")) %>%
      dplyr::mutate(source=stringr::str_remove_all(source, "[:digit:]"))

    #process new variables if they exist
    if (length(variables_required$var_name[(!variables_required$var_name %in% suppressMessages(available_variables()$var_name))])>=1 ){

      new_vars <- variables_required %>%  dplyr::filter(!var_name %in% suppressMessages(available_variables()$var_name))
      variables_required <-  variables_required %>%  dplyr::filter(var_name %in% suppressMessages(available_variables()$var_name))
      new_vars_processed <- suppressMessages(available_variables() %>% dplyr::filter(is.null(var_name)))

           # npr/kuhr code combinations
      if(any(stringr::str_detect(new_vars$var_name,"="))){
        code_combo_new_vars <- dplyr::tibble(measure = rep("new",length(new_vars$var_name[stringr::str_detect(new_vars$var_name,"=")])),
                                      subscale = rep(NA,length(new_vars$var_name[stringr::str_detect(new_vars$var_name,"=")])),
                                      questionnaire = rep(NA,length(new_vars$var_name[stringr::str_detect(new_vars$var_name,"=")])),
                                      var_name = new_vars$var_name[stringr::str_detect(new_vars$var_name,"=")],
                                      source = new_vars %>% dplyr:: filter(stringr::str_detect(new_vars$var_name,"=")) %>% .$source)
        new_vars <- new_vars %>%  dplyr::filter(!stringr::str_detect(var_name,"="))
        new_vars_processed <- new_vars_processed %>%
          dplyr::bind_rows(code_combo_new_vars)
      }

      if(any(new_vars$source != "moba")){
        stop(paste0("Could not find variable(s): ", new_vars %>% dplyr::filter(source!="moba") %>% .$var_name, ", specified as coming from
source(s):",new_vars %>% dplyr::filter(source!="moba") %>% .$source,"; check your inputs and refer to available_variables()."  ))
      }

      #MoBa specific item codes
      if(length(new_vars$var_name)>0){

         moba_new_vars <- sapply(new_vars$var_name,moba_item_finder) %>%
          tibble::enframe() %>%
          `colnames<-`(c("var_name", "questionnaire")) %>%
          dplyr::mutate(measure = ifelse(!is.na(questionnaire),"single_item",NA),
                        subscale = rep(NA,length(new_vars$var_name)),
                        source = ifelse(!is.na(questionnaire),"moba",NA))
        new_vars_processed <- new_vars_processed %>%
          dplyr::bind_rows(moba_new_vars)
      }

      #Combine new_vars objects

      if(any(is.na(new_vars_processed$source))){
        stop(paste0("Cannot find a source for variable(s): ", new_vars_processed %>% dplyr::filter(is.na(source)) %>% .$var_name, "
Did you mistype a variable name? If you think this variable should be available, please contact lauriejhannigan@gmail.com.
For a list of valid pre-processed variable names run avaliable_variables(), or check the MoBa wiki for valid item codes.") )
      }


    }


    #make reqd_vars (with new_vars_processed if exists)
    if(exists("new_vars_processed")){
      reqd_vars <- suppressMessages(available_variables(source = c("moba","npr","kuhr"))) %>%
        dplyr::filter(var_name %in% variables_required$var_name & source %in% variables_required$source) %>%
        dplyr::bind_rows(new_vars_processed)
    }else {
      reqd_vars <- suppressMessages(available_variables(source = c("moba","npr","kuhr"))) %>%
        dplyr::filter(var_name %in% variables_required$var_name & source %in% variables_required$source)
    }


  }

  # Get sources
  sources <- reqd_vars %>%
    dplyr::select(source) %>% dplyr::distinct()


  all_data_combined <- list()
  ################
  #PROCESS: MOBA
  ################

  ##Make up lookup table of filepaths, comprising defaults and any overrides
  ##NB this is done outside of the MoBa-specific section because curate_npr/kuhr
  ##need to inherit any changes to moba mbrn and sv_info filepaths

  make_moba_filepath <- function(x, name=NULL){
    if(is.null(name)){
      return(paste0(moba_data_root_dir,"PDB",PDB,"_",x,"_v",moba_data_version,".sav"))
    }else{
      return(paste0(moba_data_root_dir,name))
    }
  }

  moba_filepaths <- dplyr::tibble(MBRN = make_moba_filepath("MBRN_541"),
                                  SV_INFO = make_moba_filepath("SV_INFO"),
                                  Q1 = make_moba_filepath("Q1"),
                                  Q3 = make_moba_filepath("Q3"),
                                  Q4_6months = make_moba_filepath("Q4_6months"),
                                  Q5_18months = make_moba_filepath("Q5_18months"),
                                  Q5yrs = make_moba_filepath("Q5yrs"),
                                  Q6_3yrs = make_moba_filepath("Q6_3yrs"),
                                  Q7yrs = make_moba_filepath("Q7yrs"),
                                  Q8yrs = make_moba_filepath("Q8yrs"),
                                  QF = make_moba_filepath("QF"),
                                  Far2 = make_moba_filepath("Far2"),
                                  Q14yM = make_moba_filepath("Q14yM"),
                                  Q14yB = make_moba_filepath("Q14yB")) %>%
    t() %>% as.data.frame %>%
    tibble::rownames_to_column() %>%
    `colnames<-`(c("questionnaire","filepath"))

  if(any(stringr::str_detect(override_filenames, "="))){

    filepaths_new <- override_filenames %>%
      dplyr::as_tibble() %>%
      tidyr::separate(value, into = c("questionnaire","name"), sep="=") %>%
      dplyr::mutate_all(stringr::str_trim) %>%
      dplyr::mutate(filepath = make_moba_filepath(questionnaire,name))

    if(any(!stringr::str_detect(filepaths_new$filepath,".sav"))){
      stop("Only SPSS files supported as inputs at present; you have either specified a non-SPSS file
in override_filenames or else forgotten the '.sav' file extension.")
    }


    moba_filepaths$filepath[match(filepaths_new$questionnaire, moba_filepaths$questionnaire)] <- filepaths_new$filepath

  }


  if(any(sources$source == "moba")){

    message("\nProcessing MoBa variables...")

    moba_vars <-
      suppressMessages(
        suppressWarnings(
          reqd_vars %>%
            dplyr::filter(source =="moba") %>%
            dplyr::left_join(phenotools::moba) %>%
            dplyr::mutate(items=ifelse(measure=="single_item",var_name,items),
                          helper=ifelse(measure=="single_item","single_item",helper))))

    ##W2W consistent items only?
    if(consistent_items == TRUE){
      moba_vars <- moba_vars %>%
        dplyr::mutate(items=ifelse(is.na(consistent),items,consistent ))
    }

    ##Warn mchat
    if(any(stringr::str_detect(moba_vars$var_name, "mchat")) & completion_threshold<1){
      message(paste0("\nThe M-CHAT scale variables are designed to be used when all items are non-missing,
so mean imputation (the default when completion_threshold<1, may not be advisable.
Consider re-running with completion_threshold=1 for these variables: ", paste0(moba_vars %>%
                                                                                 dplyr::filter(stringr::str_detect(var_name, "mchat")) %>% .$var_name, collapse= ", "), ".\n") )}

    ##Warn asq/cdq 5 yrs
    if(any(stringr::str_detect(moba_vars$var_name, "asq_mot_c_5yr|cdi_mot_c_5yr")) & completion_threshold<1){
      message(paste0("\nBe aware that asq_mot_c_5yr and cdi_mot_c_5yr ARE IDENTICAL AND SHOULD NOT BE TREATED AS SEPARATE VARIABLES.
Details: Motor items at 5 years come from the Child Development Inventory, not the ASQ (as listed in
the MoBa instrument synthesis).As such, they have a different response set (Yes, No), and scores on
this variable are not comparable to scores on the ASQ in raw form. Phenotools allows the scale to be
called by either the asq_ or cdi_ prefix for consistency with both MoBa documentation and the true
source of the items.") )}


   ##Create data.frame of PREG_IDs to aggregate created variables
    mbrn <-
      haven::read_spss(moba_filepaths %>% dplyr::filter(questionnaire=="MBRN") %>% .$filepath ) %>%
      dplyr::select(preg_id = dplyr::matches("PREG_ID"),BARN_NR,birth_yr =FAAR) %>%
      dplyr::mutate(preg_id = as.integer(preg_id))
    ##Add M_ID and F_ID variables from SV info
    sv_info <-
      haven::read_spss(moba_filepaths %>% dplyr::filter(questionnaire=="SV_INFO") %>% .$filepath )%>%
      dplyr::select(preg_id = dplyr::matches("PREG_ID"),
                    m_id = dplyr::matches("M_ID"),
                    f_id = dplyr::matches("F_ID"))%>%
      dplyr::mutate(preg_id = as.integer(preg_id))

    moba_data <-
      suppressMessages(
        suppressWarnings(
          sv_info %>%
            dplyr::left_join(mbrn)))%>%
      dplyr::mutate(preg_id=as.character(preg_id),
                    m_id=as.character(stringr::str_replace_all(m_id, stringr::fixed (" "), "")),
                    f_id=as.character(stringr::str_replace_all(f_id, stringr::fixed (" "), ""))) %>%
      dplyr::mutate_if(is.character, list(~dplyr::na_if(.,"")))

    #Get item-level datasets and combine

    for(q in unique(moba_vars$questionnaire)){

      message(paste0("\nLoading data from questionnaire ",q,", which is number ",match(q,unique(moba_vars$questionnaire))," of ", length(unique(moba_vars$questionnaire)) ))

      if(q %in% c("Q1","Q3","QF")){
        suppressMessages(qvars_temp <- haven::read_spss(moba_filepaths %>% dplyr::filter(questionnaire==q) %>% .$filepath ) %>%
                           dplyr::select(preg_id = dplyr::matches("PREG_ID"),
                                         unlist(strsplit(paste0(dplyr::filter(moba_vars,questionnaire == q)$items, collapse=","),","))) %>%
                           dplyr::mutate(preg_id=as.character(preg_id)))
      }
      if(q %in% c("Far2")){
        suppressMessages(qvars_temp <- haven::read_spss(moba_filepaths %>% dplyr::filter(questionnaire==q) %>% .$filepath ) %>%
                           dplyr::select(f_id = dplyr::matches("F_ID"),
                                         unlist(strsplit(paste0(dplyr::filter(moba_vars,questionnaire == q)$items, collapse=","),","))) %>%
                           dplyr::mutate(f_id=as.character(f_id)))
      }
      if(q %in% c("Q4_6months","Q5_18months","Q5yrs","Q6_3yrs","Q7yrs","Q8yrs","MBRN","Q14yB","Q14yM")){
        suppressMessages(qvars_temp <- haven::read_spss(moba_filepaths %>% dplyr::filter(questionnaire==q) %>% .$filepath ) %>%
                           dplyr::mutate(EE_EASsoc_dummy = NA) %>%
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

    }else{
      moba_scales_items <- moba_data %>%
        dplyr::select(preg_id:birth_yr)
    }

    #process non-scale variables with calls to relevant helper functions

    moba_other_data <- moba_data %>%
      dplyr::select(preg_id:birth_yr)

    if(nrow(moba_other_vars)>0){
      message("\nProcessing non-scale (or non-standard) MoBa vars. These are: \n\n",paste0(c(moba_other_vars$var_name), collapse="", sep="\n"))




      for(helpfun in unique(moba_other_vars$helper)){

        f <- match.fun(helpfun)
        moba_other_temp <- f(moba_other_vars,
                             moba_other_vars_long,
                             moba_data,
                             return_items,
                             completion_threshold)
        moba_other_data <-
          suppressMessages(
            suppressWarnings(
              moba_other_data %>%
                dplyr::left_join(moba_other_temp)))
      }
      message("\nProcessing of non-scale (or non-standard) MoBa variables is complete.")
    }



  # Implement return preferences

    if(return_items==T & nrow(moba_scale_vars)>0){
      moba_data_combined <-
        suppressMessages(
          suppressWarnings(list( scales =
                                   moba_scales_items[["scales"]] %>%
                                   dplyr::left_join(moba_other_data%>%
                                                      dplyr::select(-dplyr::matches("_raw|_coded"))),
                                 items =
                                   moba_scales_items[["items"]] %>%
                                   dplyr::left_join(moba_other_data %>%
                                                      dplyr::select(preg_id,m_id,f_id,BARN_NR,birth_yr,dplyr::matches("_raw|_coded"))))))
      if(out_format == "list"){

      message(
        "\nYou requested that scale items be returned (using return_items=TRUE), so
output is a list, of which the first element (\"scales\") is your scale-level
dataset, and the second (\"items\")is your item-level dataset, with \"_raw\" and
\"_coded\" (i.e., numeric) versions of all items for each scale.")

      }else{

        moba_data_combined <- moba_data_combined %>%
          purrr::reduce(dplyr::full_join, by=c("preg_id","BARN_NR","m_id","f_id"))

      message(
        "\nYou requested that scale items be returned (using return_items=TRUE)
and that the output be a 'merged_df', so your item-level dataset, with \"_raw\" and
\"_coded\" (i.e., numeric) versions of all items for each scale, has been joined
to your main dataset.")

      }

    }else{
      moba_data_combined <-
        suppressMessages(
          suppressWarnings(moba_scales_items %>%
                             dplyr::left_join(moba_other_data)))

    }

    message("\nMoBa dataset curation complete.")




    all_data_combined[["moba"]]<- moba_data_combined
  }



  #NPR
  ###################################################################################
  if(any(sources$source == "npr")){

    message("\nProcessing NPR variables...")

    if(!exists("exclusions")|
       !exists("recursive")|
       !exists("group_all")|
       !exists("dx_owner")){
      warning(
        "\nNPR data: please be aware that you are using one or more defaults from
curate_npr(). These are consequential for how the NPR data are processed, so be
sure that you are getting what you want by checking ?curate_npr and, if needs
be, re-running your curate_dataset() with additional arguments to pass to
curate_npr().")
    }

    npr_vars <-
      suppressMessages(
        suppressWarnings(
          reqd_vars %>%
            dplyr::filter(source =="npr")))


        npr_data_combined <- curate_npr(diagnoses = npr_vars$var_name,
                                        moba_filepaths = moba_filepaths,
                                        ...)


    message("\nNPR dataset curation complete.")

    all_data_combined[["npr"]]<- npr_data_combined
  }

  #KUHR
  ###################################################################################
  if(any(sources$source == "kuhr")){

    message("\nProcessing KUHR variables...")

    if(!exists("code_system")|
       !exists("primary_care_only")|
       !exists("group_all")|
       !exists("dx_owner")){
      warning(
        "\nKUHR data: please be aware that you are using one or more defaults from
curate_kuhr(). These are consequential for how the KUHR data are processed, so be
sure that you are getting what you want by checking ?curate_kuhr and, if needs
be, re-running your curate_dataset() with additional arguments to pass to
curate_kuhr().")
    }

    kuhr_vars <-
      suppressMessages(
        suppressWarnings(
          reqd_vars %>%
            dplyr::filter(source =="kuhr")))


    kuhr_data_combined <- curate_kuhr(diagnoses = kuhr_vars$var_name,
                                      moba_filepaths = moba_filepaths,
                                      ...)


    message("\nKUHR dataset curation complete.")

    all_data_combined[["kuhr"]]<- kuhr_data_combined
  }


  message("\nCurating the final dataset(s)...")

  if(out_format == "merged_df"){

      all_data_combined <- all_data_combined %>%
      purrr::reduce(dplyr::full_join)
  }
  message("\nDataset curation complete.")
  return(all_data_combined)

  if(!is.null(log)){
    logr::log_close()
  }



}

