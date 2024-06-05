#' Curate NPR data
#'
#' \code{curate_npr} is called by \code{curate_dataset}
#' when npr variables are requested - options for this function are included
#' directly in your \code{curate_dataset} call and passed on internally
#'
#'
#' This internal function is called when you list ICD-10 diagnostic codes in your
#' \code{curate_dataset} call. It's default is to work directly on a raw file
#' delivered by NPR, but since deliveries often vary in structure and content
#' in unpredictable ways, you may need to pre-process your NPR data for phenotools
#' to be able to work with it. Necessary formatting conditions are as follows:
#'
#' - Data is in .sav format
#' - One row per healthcare interaction (individuals recur across rows)
#' - NPR ID column named either "LNR" or "LOPENR" (case insensitive)
#' - Diagnoses in columns prefixed with "tilst" (usually as delivered)
#' - Columns: innDato,utDato,henvType,omsorgsniva3,SUPtype (as delivered)
#'
#' If your raw file meets these conditions you should be able to direct the function
#' to it \code{npr_filename} and \code{npr_data_root_dir} options in your
#' \code{curate_dataset}) call. If not, you should create a version of the file
#' meeting these conditions for use with phenotools.
#'
#' The next step, prior to processing diagnoses, is to link with MoBa IDs. Again,
#' the function will do this automatically if the linkage file (specified via
#'  \code{npr_linkage_filename} and \code{npr_linkage_file_root_dir}) is in the standard
#'  format, i.e., containing linkage information for all members of family trios
#'  with the columns preg_id, m_id, f_id (all case insensitive), BARN_NR, LNr, and
#'  the variable SUPtype (case insensitive) - which is either "child", "mother",
#'  or "father" depending on who is the owner of the LNr. If the linkage file is
#'  not in this format, either format the file to match or pre-link the data yourself,
#'  in which case it should be read into memory and supplied using the option
#'   \code{npr_preprocessed}
#'
#'
#' @param diagnoses ICD codes (see \code{available_variables(source=\"npr\")})
#' @param recursive also include sub-codes of codes you have provided in the
#' diagnoses field? Defaults to TRUE
#' @param exclusions codes to exclude - always non-recursive (i.e., only the
#' specific codes you provide will be excluded from individual-level summaries, not sub-codes)
#' @param group_all If TRUE, will treat all your codes as a single diagnosis
#' for the purpose of summarising; defaults to FALSE
#' @param dx_groupname If group_all is TRUE, this string is used to label the
#' vareiables pertaining to the grouped diagnoses
#' @param dx_owners Whose diagnoses do you want to count? Dataset is returned as
#' one row per pregnancy, but you can get the diagnoses relevant to any
#' combination of "child", "father", and "mother"; the default is "child"
#' @param dx_range_limits If you want only to extract diagnoses from a particular
#' range of available years, provide limits here (e.g., "c(2008,2010)" will retrieve
#' diagnoses from 2008, 2009, and 2010 only). Defaults to NULL - i.e., extract
#' from all available data
#' @param npr_full if you already have the NPR data loaded in memory, you can
#' save some time by providing the name of the R object here; note
#' that there are specific requirements for the structure of this file (see
#' Details) below.
#' @param npr_preprocessed if you have already got a version of the NPR data
#' linked with MoBa IDs loaded in memory, supply the R object name here; note
#' that there are specific requirements for the structure of this file (see
#' Details) below.
#' @param moba_data_root_dir Where is the raw MoBa phenotypic data? (default
#' is for p471)
#' @param npr_data_root_dir Where is the raw NPR phenotypic data? (default
#' is for p471)
#' @param npr_filename What is the name of the NPR file? (default
#' is for p471)
#' @param npr_linkage_file_root_dir Where is the linkage file? (default
#' is for p471)
#' @param npr_linkage_filename What is the name of the linkage file? (default
#' is for p471). Required variables are: LNr (link number); M_ID_>PDB< (maternal ID);
#' F_ID_>PDB< (paternal ID); PREG_ID_>PDB< (preg ID); BARN_NR (child number in pregnancy);
#' SUP_Type (either MOTHER/FATHER/CHILD)
#' @param PDB What is the PDB code for your TSD project? (default
#' is for p471)
#' @param moba_data_version What version is the MoBa data you are linking to? Defaults to
#' 12
#' @param moba_filepaths Allows curate_dataset to pass on an amended list of filepaths
#' @export
#' @importFrom dplyr "%>%"
#' @importFrom lubridate "%--%"


curate_npr <- function(diagnoses,
                       recursive=TRUE,
                       exclusions=NULL,
                       group_all =FALSE,
                       dx_groupname =NULL,
                       dx_owners = c("child"),
                       dx_range_limits = NULL,
                       npr_full = NULL,
                       npr_preprocessed = NULL,
                       moba_data_root_dir= "//ess01/P471/data/durable/data/MoBaPhenoData/PDB2306_MoBa_V12/SPSS/",
                       npr_data_root_dir= "//ess01/P471/data/durable/data/NPR/processed/",
                       npr_filename="npr2024.sav",
                       npr_linkage_file_root_dir = "//ess01/P471/data/durable/data/Linkage_files/NPR_link/",
                       npr_linkage_filename = "PDB2306_kobling_npr2024_mor_far_barn.sav",
                       PDB="2306",
                       moba_data_version = 12,
                       moba_filepaths= NULL,
                       ...)
{

  ## Initial warning about checking available NPR codes...
  warning(
    "\nAll diagnostic codes from across all chapters of the ICD-10 are theoretically able to be retrieved and processed by the phenotools package. However, we have no way of knowing which codes *should* be available to you in your TSD project. Check with your project administrator to make sure you are aware which diagnostic codes are available as primary diagnoses in your project. If you have requested codes that were not primary diagnoses in your project's data delivery, but which have not been properly censored and so occur as secondary diagnoses in your project's dataset, phenotools will return them without making a distinction.\nTo avoid reporting incorrect counts of diagnoses, it is your responsibility to ensure that requested codes are available in your project.\n")

  ## LOADING FULL NPR DATASET INTO MEMORY
  ############################

  # Pre-proceesed data - skip load and linkage
  if(!is.null(npr_preprocessed)){

    message(
      "\n\nWorking with pre-processed NPR file...")

    if(any(!c("preg_id", "BARN_NR", "m_id","f_id","FAAR", "dx_owner","LNr")
           %in% names(npr_preprocessed))){
      stop("Pre-processed NPR file is missing key variables, or they are not named
      in a way that phenotools can recognise. Check ?curate_npr for the very
      specific criteria that must apply to preprocessed NPR data.")
    }


    npr_processed <- npr_preprocessed %>%
      dplyr::select(preg_id, BARN_NR, m_id,f_id,FAAR, dx_owner,LNr) %>%
      dplyr::distinct()

    npr_link_moba <- npr_preprocessed %>%
      dplyr::select(preg_id, BARN_NR, m_id,f_id,FAAR, dx_owner,LNr)

    npr_full <-npr_preprocessed%>%
      dplyr::mutate_at(dplyr::vars(tidyr::starts_with("tilst|NCMP|NCSP")), list(~stringr::str_remove_all(.,"[[:punct:]]"),
                                                                                ~stringr::str_trim(.,"both"),
                                                                                ~stringr::str_replace_all(., stringr::fixed(" "), ""))) %>% #Remove non alphanumeric values in all tilst- variables
      dplyr::mutate_at(dplyr::vars(tidyr::starts_with("tilst")), list(~stringr::str_sub(.,end=4))) %>%
      dplyr::mutate_at(dplyr::vars(tidyr::starts_with("tilst|NCMP|NCSP")), dplyr::na_if, "") %>%
      dplyr::mutate_at(dplyr::vars(dplyr::matches("Dato")), as.Date.character ) %>%
      dplyr::rename("LNr"=dplyr::matches("lnr|lopenr"),
                    "omsorgsniva3" = dplyr::matches("omsorgsniva$"),
                    "henvTypeVurd" = dplyr::matches("henvType$")) %>%
      dplyr::mutate(NCMP_x = NA, # Dummy cols in case these code cols are not included
                    NCSP_x = NA) # Dummy cols in case these code cols are not included
    mem_preserve<-1
  }else{

    #Full data loaded - skip load
    if(!is.null(npr_full)){
      mem_preserve<-0
    }else {
      #Read in the NPR data and filter
      message(
        "\n\nReading in NPR data file (this is a large file so expect a wait of >2 mins)...")

      mem_preserve <-1

      npr_full <- haven::read_spss(paste0(npr_data_root_dir,npr_filename)) %>%
        dplyr::mutate_at(dplyr::vars(tidyr::starts_with("tilst|NCMP|NCSP")), list(~stringr::str_remove_all(.,"[[:punct:]]"),
                                                                                  ~stringr::str_trim(.,"both"),
                                                                                  ~stringr::str_replace_all(., stringr::fixed(" "), ""))) %>% #Remove non alphanumeric values in all tilst- variables
        dplyr::mutate_at(dplyr::vars(tidyr::starts_with("tilst")), list(~stringr::str_sub(.,end=4))) %>%
        dplyr::mutate_if(is.character, dplyr::na_if, "") %>%
        dplyr::mutate_at(dplyr::vars(dplyr::matches("Dato")), as.Date.character )%>%
        dplyr::rename("LNr"= tidyr::ends_with("Nr"),
                      "omsorgsniva3" = dplyr::matches("omsorgsniva$"),
                      "henvTypeVurd" = dplyr::matches("henvType$")) %>%
        dplyr::mutate(NCMP_x = NA, # Dummy cols in case these code cols are not included
                      NCSP_x = NA) # Dummy cols in case these code cols are not included

    }
    ############################
    ##LINKAGE
    ############################

    message("\nMerging NPR data with MoBa IDs...")
    #Read in the linkage file, sv_infor, and mbrn
    link <- haven::read_spss(paste0(npr_linkage_file_root_dir,npr_linkage_filename)) %>%
      dplyr::select(preg_id = dplyr::matches("preg_id"),
                    m_id = dplyr::matches("M_ID"),
                    f_id = dplyr::matches("f_id"),
                    BARN_NR,
                    dx_owner = SUP_Type,
                    LNr)%>%
      dplyr::mutate(f_id = stringr::str_replace_all(f_id, stringr::fixed(" "), ""),
                    m_id = stringr::str_replace_all(m_id, stringr::fixed(" "), ""),
                    dx_owner = tolower(stringr::str_remove(dx_owner, "SU2PT_"))) %>%
      dplyr::na_if("")

    if(exists("moba_filepaths")){
      mbrn <-
        haven::read_spss(moba_filepaths %>% dplyr::filter(questionnaire=="MBRN") %>% .$filepath ) %>%
        dplyr::select(preg_id = dplyr::matches("PREG_ID"),BARN_NR,FAAR)
      ##Add M_ID and F_ID variables from SV info
      sv_info <-
        haven::read_spss(moba_filepaths %>% dplyr::filter(questionnaire=="SV_INFO") %>% .$filepath )%>%
        dplyr::select(preg_id = dplyr::matches("preg_id"),
                      m_id = dplyr::matches("M_ID"),
                      f_id = dplyr::matches("f_id") )%>%
        dplyr::mutate(f_id = stringr::str_replace_all(f_id, stringr::fixed(" "), ""),
                      m_id = stringr::str_replace_all(m_id, stringr::fixed(" "), "")) %>%
        dplyr::na_if("")

    }else{
      mbrn <- haven::read_spss(paste0(moba_data_root_dir,"PDB",PDB,"_MBRN_541_v",moba_data_version,".sav")) %>%
        dplyr::select(preg_id = dplyr::matches("PREG_ID"),BARN_NR,FAAR)

      sv_info <- haven::read_spss(paste0(moba_data_root_dir,"PDB",PDB,"_SV_INFO_v",moba_data_version,".sav")) %>%
        dplyr::select(preg_id = dplyr::matches("preg_id"),
                      m_id = dplyr::matches("M_ID"),
                      f_id = dplyr::matches("f_id") )%>%
        dplyr::mutate(f_id = stringr::str_replace_all(f_id, stringr::fixed(" "), ""),
                      m_id = stringr::str_replace_all(m_id, stringr::fixed(" "), "")) %>%
        dplyr::na_if("")

    }



    #Join together

    npr_link_child <- suppressMessages(sv_info %>%
                                         dplyr::left_join(mbrn) %>%
                                         dplyr::left_join(link %>%
                                                            dplyr::filter(dx_owner=="child") %>%
                                                            dplyr::select(-m_id,-f_id)) %>%
                                         dplyr::mutate(dx_owner="child")) #Ensures that those without linkage are
    #identifiable within a family

    npr_link_mother <- suppressMessages(sv_info %>%
                                          dplyr::left_join(mbrn) %>%
                                          dplyr::left_join(link %>%
                                                             dplyr::filter(dx_owner=="mother") %>%
                                                             dplyr::select(-preg_id,-f_id,-BARN_NR)) %>%
                                          dplyr::mutate(dx_owner="mother")) #Ensures that those without linkage are
    #identifiable within a family
    npr_link_father <- suppressMessages(sv_info %>%
                                          dplyr::left_join(mbrn) %>%
                                          dplyr::left_join(link %>%
                                                             dplyr::filter(dx_owner=="father") %>%
                                                             dplyr::select(-m_id,-preg_id,-BARN_NR)) %>%
                                          dplyr::mutate(dx_owner="father")) #Ensures that those without linkage are
    #identifiable within a family

    npr_link_moba <- suppressMessages(npr_link_child %>%
                                        dplyr::bind_rows(npr_link_mother) %>%
                                        dplyr::bind_rows(npr_link_father) %>%
                                        dplyr::arrange(preg_id, BARN_NR))

    rm(link,mbrn,sv_info,npr_link_child,npr_link_mother,npr_link_father)



    #Collapse diagnoses, count number of diagnoses, earliest, all dates of relevant diags per indiv
    npr_processed <- npr_link_moba %>%
      dplyr::distinct()


  }


  ############################
  #### INPUTS
  ############################

  # To handle new combinations of NPR codes:

  message("\nSummarising NPR information at the individual level...")
  if(any(stringr::str_detect(diagnoses, "="))){

    diagnoses_std <- diagnoses[!stringr::str_detect(diagnoses, "=")] %>%
      toupper()

    diagnoses_new <- diagnoses[stringr::str_detect(diagnoses, "=")] %>%
      dplyr::as_tibble() %>%
      tidyr::separate(value, into = c("dx_groupname","diagnoses"), sep="=") %>%
      dplyr::mutate_all(stringr::str_trim) %>%
      dplyr::mutate(diagnoses = strsplit(stringr::str_remove_all(toupper(diagnoses)," "),","))


  }else {
    diagnoses_std<- diagnoses %>%
      toupper()
  }
  if(recursive==TRUE){

    if(length(diagnoses_std)>0){
      diag_std_tmp <- phenotools::npr %>%
        dplyr::filter(stringr::str_detect(level4,paste(diagnoses_std, collapse = "|"))|
                        stringr::str_detect(level3,paste(diagnoses_std, collapse = "|"))|
                        stringr::str_detect(level2,paste(diagnoses_std, collapse = "|"))|
                        stringr::str_detect(chapter,paste(diagnoses_std, collapse = "|"))) %>%
        dplyr::mutate(diag=ifelse(!is.na(level4), level4,
                                  ifelse(!is.na(level3),level3,
                                         ifelse(!is.na(level2),level2,chapter)))) %>%
        dplyr::filter(!diag %in% exclusions)
    }else {
      diag_std_tmp <- NULL
    }


    if(exists("diagnoses_new")){
      diag_new_tmp <- data.frame()

      for(i in 1:nrow(diagnoses_new)){

        diag_new_tmp_tmp <- phenotools::npr %>%
          dplyr::filter(stringr::str_detect(level4,paste(diagnoses_new$diagnoses[[i]], collapse = "|"))|
                          stringr::str_detect(level3,paste(diagnoses_new$diagnoses[[i]], collapse = "|"))|
                          stringr::str_detect(level2,paste(diagnoses_new$diagnoses[[i]], collapse = "|"))|
                          stringr::str_detect(chapter,paste(diagnoses_new$diagnoses[[i]], collapse = "|"))) %>%
          dplyr::mutate(diag=ifelse(!is.na(level4), level4,
                                    ifelse(!is.na(level3),level3,
                                           ifelse(!is.na(level2),level2,chapter))),
                        dx_groupname = diagnoses_new$dx_groupname[i]) %>%
          dplyr::filter(!diag %in% exclusions)

        diag_new_tmp <- rbind(diag_new_tmp,diag_new_tmp_tmp)
      }
    }
  } else{

    if(length(diagnoses_std)>0){
      diag_std_tmp <- phenotools::npr %>%
        dplyr::mutate(diag=ifelse(!is.na(level4), level4,
                                  ifelse(!is.na(level3),level3,
                                         ifelse(!is.na(level2),level2,chapter)))) %>%
        dplyr::filter(diag %in% diagnoses & !diag %in% exclusions)
    }else {
      diag_std_tmp <- NULL
    }
    if(exists("diagnoses_new")){

      diag_new_tmp <- data.frame()
      for(i in 1:nrow(diagnoses_new)){

        diag_new_tmp_tmp <- phenotools::npr %>%
          dplyr::mutate(diag=ifelse(!is.na(level4), level4,
                                    ifelse(!is.na(level3),level3,
                                           ifelse(!is.na(level2),level2,chapter)))) %>%
          dplyr::filter(diag %in% diagnoses_new$diagnoses[[i]]& !diag %in% exclusions) %>%
          dplyr::mutate(dx_groupname = diagnoses_new$dx_groupname[i])


        diag_new_tmp <- rbind(diag_new_tmp,diag_new_tmp_tmp)
      }
    }
  }


  if(exists("diag_new_tmp")){
    if(length(diag_std_tmp$diag)==0 & length(diag_new_tmp$diag)==0){
      stop("No valid NPR diagnoses remaining. Review your inputs and use available_variables(source=\"npr\") to get valid codes.")
    }
  }else{
    if(length(diag_std_tmp$diag)==0 ){
      stop("No valid NPR diagnoses remaining. Review your inputs and use available_variables(source=\"npr\") to get valid codes.")
    }
  }


  ################################
  ## NPR processing

  # Implement date range

  if(is.null(dx_range_limits)){
    dx_yrs = seq(min(npr_full$aar),max(npr_full$aar),1)
  } else {
    dx_yrs = seq(dx_range_limits[[1]],dx_range_limits[[2]],1)
    # Check validity of range:
    if(min(dx_yrs)<min(npr_full$aar)|max(dx_yrs)>max(npr_full$aar)){
      dx_yrs = seq(min(npr_full$aar),max(npr_full$aar),1)
      warning(paste0("The dx_range_limits you have supplied fall outside the range of years
            available in the data. Defaulting to all available years (",min(npr_full$aar), " to ",max(npr_full$aar),").
            Re-run and specify years within this range if you want to refine further"))
    }
  }

  # Three cases: group_all=TRUE, group_all=FALSE, and new_dx (can occur with either of the previous)

  # The consistent portion as a function:

  process_npr <- function (d){
    npr_reduced_tmp <- suppressMessages(npr_full %>%
                                          dplyr::filter_at(dplyr::vars(tidyr::starts_with("tilst")), dplyr::any_vars( stringr::str_detect(. ,d))) %>%
                                          dplyr::filter(aar %in% dx_yrs) %>%
                                          dplyr::left_join(npr_link_moba %>%
                                                             dplyr::select(LNr, FAAR,dx_owner) %>%
                                                             dplyr::distinct())) %>%
      dplyr::mutate(hosp = lubridate::as.duration(innDato %--% utDato) / lubridate::ddays(1),
                    innDato = lubridate::round_date(lubridate::as_date(innDato),unit="month"),
                    utDato = lubridate::round_date(lubridate::as_date(utDato),unit="month"),
                    hosp_bin = ifelse(hosp==0,0,1),
                    inpatient = factor(dplyr::case_when(omsorgsniva3==1 ~ "Yes",
                                                        omsorgsniva3==2 ~ "Day only",
                                                        omsorgsniva3==3 ~ "No (outpatient)")),
                    referral = factor(dplyr::case_when(henvTypeVurd==1 ~ "Assessment",
                                                       henvTypeVurd==2 ~ "Treatment and further assesment",
                                                       henvTypeVurd==3 ~ "Control",
                                                       henvTypeVurd==4 ~ "Immediate help",
                                                       henvTypeVurd==5 ~ "Healthy newborn",
                                                       henvTypeVurd==6 ~ "Pregnancy",
                                                       henvTypeVurd==7 ~ "Care, accommodation or other")),
                    childage_at_yrs = aar - FAAR) %>%
      dplyr::mutate_at(dplyr::vars(tidyr::starts_with("tilst")), dplyr::na_if, "") %>%
      tidyr::unite(other_codes, dplyr::starts_with("tilst"), sep=",", na.rm = TRUE) %>%
      dplyr::mutate(other_codes, stringr::str_remove_all(other_codes, d)) %>%
      tidyr::unite(ncmp_codes, dplyr::starts_with("ncmp"), sep=",", na.rm = TRUE) %>%
      tidyr::unite(ncsp_codes, dplyr::starts_with("ncsp"), sep=",", na.rm = TRUE) %>%
      dplyr::group_by(LNr) %>%
      dplyr::arrange(utDato) %>%
      dplyr::summarise(received_dx = "yes",
                       received_dx_2x = ifelse(dplyr::n()>1, "yes",NA),
                       n_times_dx = dplyr::n(),
                       date_first_dx = min(utDato),
                       year_first_dx = min(aar),
                       childageyrs_first_dx = min(childage_at_yrs),
                       dates_all_dx = paste0(utDato, collapse="; "),
                       years_all_dx = paste0(aar, collapse="; "),
                       childageyrs_all_dx = paste0(childage_at_yrs, collapse="; "),
                       inpatient_all_dx = paste0(inpatient, collapse="; "),
                       n_hosp_dx = sum(hosp_bin),
                       referraltype_all_dx = paste0(referral, collapse="; "),
                       longest_hosp_dx = max(hosp),
                       total_hosp_dx = sum(hosp),
                       otherdx_all_dx = paste0(other_codes, collapse="; "),
                       ncmp_all_dx = paste0(ncmp_codes, collapse="; "),
                       ncsp_all_dx = paste0(ncsp_codes, collapse="; ")) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(otherdx_all_dx= stringr::str_remove_all(otherdx_all_dx, ",,|,;|;,"))

    return(npr_reduced_tmp)
  }

  #Ungrouped dx case (there must be some std diagnoses)

  if(group_all==FALSE&!is.null(diag_std_tmp)){
    for(d in diag_std_tmp$diag){

      message(paste0(
        "\nProcessing diagnosis code: ",d,", for variable set number ",match(d,unique(diag_std_tmp$diag)), " of ",length(diag_std_tmp$diag)+
          if(exists("diagnoses_new")){nrow(diagnoses_new)}else{0} ))

      npr_processed_tmp <- process_npr(d) %>%
        dplyr::rename_at(dplyr::vars(dplyr::matches("_dx")), ~paste(.,d,"npr",sep="_"))

      npr_processed <- suppressMessages(dplyr::left_join(npr_processed, npr_processed_tmp))

    }
  }else {

    #Grouped dx case (there must be some standard diagnoses)

    if(!is.null(diag_std_tmp)){

      d <- paste0(diag_std_tmp$diag, collapse = "|")

      message(paste0(
        "\nProcessing diagnosis codes: ",d," as a group because option group_all is set to TRUE.
If you wanted these codes processed individually, re-run with group_all=FALSE.
Resulting variables will be labelled with ", if(!is.null(dx_groupname)){dx_groupname}else{d},"."))

      npr_processed_tmp <- process_npr(d) %>%
        dplyr::rename_at(dplyr::vars(dplyr::matches("_dx")), ~paste(.,if(!is.null(dx_groupname)){dx_groupname}else{d},"npr",sep="_"))

      npr_processed <- suppressMessages(dplyr::left_join(npr_processed, npr_processed_tmp))
    }


  }
  #New dx case (grouping applied by default so no split here)

  if(exists("diagnoses_new")){

    for( dx in unique(diagnoses_new$dx_groupname)){

      d <- paste0(diag_new_tmp %>% dplyr::filter(dx_groupname==dx) %>% .$diag, collapse = "|")

      message(paste0(
        "\nProcessing diagnosis codes ",paste0(diag_new_tmp %>% dplyr::filter(dx_groupname==dx) %>% .$diag, collapse = ", ")," as a group; resulting variables will be labelled with ", dx,". If you only wanted the exact codes specified in your input, you may want need to re-run with 'recursive=FALSE'. Other curate_npr options are applied as specified in your 'curate_dataset' call, or at their defaults (see ?curate_npr)."))

      npr_processed_tmp <- process_npr(d) %>%
        dplyr::rename_at(dplyr::vars(dplyr::matches("_dx")), ~paste(.,dx,"npr",sep="_"))

      npr_processed <- suppressMessages(dplyr::left_join(npr_processed, npr_processed_tmp))

    }

  }

  #Tidy up to preserve RAM berfore potential reshape (if more than one dx_owner)


  if(mem_preserve>0){
    rm(npr_full)
  }

  #Drop all-NA columns (occur when impossible diagnostic codes get included

  not_all_na <- function(x) {!all(is.na(x))}

  npr_processed <- npr_processed %>%
    dplyr::select(where(not_all_na)) %>%
    dplyr::filter(dx_owner %in% dx_owners) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::contains("received_dx")),
                     list( ~ dplyr::case_when(!is.na(LNr)&is.na(.) ~ "no",
                                              !is.na(LNr)&!is.na(.) ~ "yes",
                                              is.na(LNr) ~ .))) %>%
    dplyr::select(-LNr) %>%
    dplyr::mutate(preg_id = as.character(preg_id)) %>%
    dplyr::rename(birth_yr = FAAR)

  if(length(dx_owners)>1)
  {

    npr_processed <- suppressMessages(suppressWarnings(npr_processed %>%
                                                         tidyr::gather(key=var_type, val=val, -preg_id:-dx_owner) %>%
                                                         tidyr::unite(var_type_owner, c("var_type","dx_owner")) %>%
                                                         tidyr::spread(var_type_owner, val)))
  }

  if(any(stringr::str_detect(dx_owners, "mother|father"))){
    message(
      "\nRemember, this dataset has one row per MoBa child (unique by a comination of
preg_id and BARN_NR). Therefore, counts of parental diagnoses based on these data
will be inflated (some parents appear in multiple rows). For accurate counts, you
need to restrict to unique m_id/f_ids.")
  }


  #String replace commas with | to avoid problems with write.csv

  npr_processed <-  npr_processed %>%
    dplyr::mutate_if(is.character, list(~stringr::str_replace_all(.,",","|") ))


  if(exists("npr_link_moba")){
    rm(npr_link_moba)
  }

  message("\nNPR data processing complete.")
  return(npr_processed)

  #############################

}



