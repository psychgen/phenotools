#' Curate kuhr data
#'
#' \code{curate_kuhr} is called by \code{curate_dataset}
#' when kuhr variables are requested
#'
#'
#' Detailed description...
#'
#' @param diagnoses Diagnostic codes (see \code{available_variables(source=\"npr\")})
#' @param code_system can be "ICPC" (the default) or "ICD10"
#' @param primary_care_only the default is to use only primary care data sources
#' from kuhr (so this option is set to TRUE); if you have other data sources in
#' your kuhr datafile, set this to FALSE
#' @param recursive also include sub-codes of codes you have provided in the
#' diagnoses field? Defaults to TRUE (only used with code_system = "ICD10")
#' @param exclusions codes to exclude - always non-recursive (i.e., only the
#' specific codes you provide will be excluded from individual-level summaries, not sub-codes)
#' (only used with code_system = "ICD10")
#' @param group_all If TRUE, will treat all your codes as a single diagnosis
#' for the purpose of summarising; defaults to FALSE
#' @param dx_groupname If group_all is TRUE, this string is used to label the
#' vareiables pertaining to the grouped diagnoses
#' @param dx_owners Whose diagnoses do you want to count? Dataset is returned as
#' one row per pregnancy, but you can get the diagnoses relevant to any
#' combination of "child", "father", and "mother"; the default is "child"
#' @param kuhr_full if you already have the KUHR data loaded in memory, you can
#' save some time by providing the name of the R object here; note
#' that there are specific requirements for the structure of this file (see
#' Details) below.
#' @param moba_data_root_dir Where is the raw MoBa phenotypic data? (default
#' is for p471)
#' @param kuhr_data_root_dir Where is the raw NPR phenotypic data? (default
#' is for p471)
#' @param kuhr_filenames_override If the KUHR filenames are not YEAR_Data_KUHR.csv,
#' what are they?
#' @param linkage_file_root_dir Where are the linkage files? (default
#' is for p471)
#' @param kuhr_linkage_filename What is the name of the linkage file? (default
#' is for p471)
#' @param PDB What is the PDB code for your TSD project? (default
#' is for p471)
#' @param moba_data_version What version is the MoBa data you are linking to? Defaults to
#' 12
#' @export
#' @importFrom dplyr "%>%"
#' @importFrom lubridate "%--%"


curate_kuhr <- function(diagnoses,
                        code_system="ICPC",
                        primary_care_only=TRUE,
                        recursive=TRUE,
                        exclusions=NULL,
                        group_all =FALSE,
                        dx_groupname =NULL,
                        dx_owners = c("child"),
                        kuhr_full = NULL,
                        moba_data_root_dir= "//tsd-evs/p471/data/durable/data/MoBaPhenoData/PDB2306_MoBa_V12/SPSS/",
                        kuhr_data_root_dir= "//tsd-evs/p471/data/durable/data/KUHR/KUHR_csv/",
                        kuhr_filenames_override=NULL,
                        linkage_file_root_dir = "//tsd-evs/p471/data/durable/data/Linkage files/",
                        kuhr_linkage_filename = "PDB2306_kobling_KUHR.sav",
                        PDB="2306",
                        moba_data_version = 12)
{


  #Full data loaded - skip load
  if(!is.null(kuhr_full)){
    mem_preserve<-0
    kuhrdata <-kuhr_full
  }else {
    mem_preserve <-1
    ## LOADING FULL KUHR DATASET INTO MEMORY
    ############################
    kuhrdata <- data.frame()
    problems <- data.frame()

    if(!is.null(kuhr_filenames_override)){
      kuhrfile_list <- kuhr_filenames_override
    }else{
      kuhrfile_list <- list.files(kuhr_data_root_dir)[stringr::str_detect(list.files(kuhr_data_root_dir), "Data_KUHR.csv")]
    }

    for(kuhrfile in kuhrfile_list){


      message(paste0("\nReading KUHR data from year: ", stringr::str_sub(kuhrfile,end=4),"..."))
      tmp <- suppressWarnings(suppressMessages(readr::read_delim(paste0(kuhr_data_root_dir,kuhrfile), delim=";")))
      problem_rows <- tmp %>% dplyr::slice(readr::problems(tmp)$row) %>%
        dplyr::mutate(source=kuhrfile)
      if(nrow(problem_rows)>0){
        tmp <-  tmp %>% dplyr::slice(-readr::problems(tmp)$row)
      }

      if(primary_care_only =="TRUE"){
        tmp <- tmp %>%
          dplyr::filter(DIAGNOSE_KODEVERK=="ICPC-2") #Check that this is most efficient/accurate means
      }else{
        stop("Functionality to incorporate KUHR data from sources other than primary care is in development;
currently this function can only run with the primary_care_only argument set to TRUE")
      }
      problems<- rbind(problems, problem_rows) #1215 misreads out of 20 milion rows - just drop these for now
      kuhrdata<- rbind(kuhrdata, tmp)

    }

    message(paste0("\n",nrow(problems)," misreads from a total of ", nrow(kuhrdata)+nrow(problems), " (",
                   format(round(nrow(problems)/(nrow(kuhrdata)+nrow(problems)),5),scientific = FALSE),
                   "%). These will be dropped..."))

    rm(tmp, problem_rows)

    # Remove nonUTF characters from var names

  }

  colnames(kuhrdata) <- iconv(names(kuhrdata),"UTF-8", "UTF-8",sub='')


  ## Do we care about rows (i.e.,contacts) with no diagnoses? These will be ignored (and may as well be explicitly
  ## dropped) if using the npr approach of curating by code(s)

  ############################
  ##LINKAGE
  ############################

  message("\nMerging KUHR data with MoBa IDs...")
  #Read in the linkage file, sv_infor, and mbrn
  link <- haven::read_spss(paste0(linkage_file_root_dir,kuhr_linkage_filename)) %>%
    dplyr::select(preg_id = dplyr::matches("preg_id"),
                  m_id = dplyr::matches("M_ID"),
                  f_id = dplyr::matches("f_id"),
                  BARN_NR,
                  dx_owner = SUP_Type,
                  LNr = PASIENTLOPENUMMER)%>%
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

    sv_info <- haven::read_spss(paste0(moba_data_root_dir,"PDB",PDB,"_SV_INFO_v",moba_data_version,"_old_version.sav")) %>%
      dplyr::select(preg_id = dplyr::matches("preg_id"),
                    m_id = dplyr::matches("M_ID"),
                    f_id = dplyr::matches("f_id") )%>%
      dplyr::mutate(f_id = stringr::str_replace_all(f_id, stringr::fixed(" "), ""),
                    m_id = stringr::str_replace_all(m_id, stringr::fixed(" "), "")) %>%
      dplyr::na_if("")

  }



  #Join together

  kuhr_link_child <- suppressMessages(sv_info %>%
                                        dplyr::left_join(mbrn) %>%
                                        dplyr::left_join(link %>%
                                                           dplyr::filter(dx_owner=="child") %>%
                                                           dplyr::select(-m_id,-f_id)))

  kuhr_link_mother <- suppressMessages(sv_info %>%
                                         dplyr::left_join(mbrn) %>%
                                         dplyr::left_join(link %>%
                                                            dplyr::filter(dx_owner=="mother") %>%
                                                            dplyr::select(-preg_id,-f_id,-BARN_NR)))

  kuhr_link_father <- suppressMessages(sv_info %>%
                                         dplyr::left_join(mbrn) %>%
                                         dplyr::left_join(link %>%
                                                            dplyr::filter(dx_owner=="father") %>%
                                                            dplyr::select(-m_id,-preg_id,-BARN_NR)))

  kuhr_link_moba <- suppressMessages(kuhr_link_child %>%
                                       dplyr::bind_rows(kuhr_link_mother) %>%
                                       dplyr::bind_rows(kuhr_link_father) %>%
                                       dplyr::arrange(preg_id, BARN_NR))

  rm(link,mbrn,sv_info,kuhr_link_child,kuhr_link_mother,kuhr_link_father)



  #Collapse diagnoses, count number of diagnoses, earliest, all dates of relevant diags per indiv
  kuhr_processed <- kuhr_link_moba %>%
    dplyr::distinct()





  ############################
  #### INPUTS
  ############################
  message("\nSummarising KUHR information at the individual level...")


  # To handle new combinations of codes:

  if(any(stringr::str_detect(diagnoses, "="))){

    diagnoses_std <- diagnoses[!stringr::str_detect(diagnoses, "=")] %>%
      toupper()

    diagnoses_new <- diagnoses[stringr::str_detect(diagnoses, "=")] %>%
      dplyr::as_tibble() %>%
      tidyr::separate(value, into = c("dx_groupname","diagnoses"), sep="=") %>%
      dplyr::mutate_all(stringr::str_trim) %>%
      dplyr::mutate(diagnoses = strsplit(stringr::str_remove_all(toupper(diagnoses)," "),","))

    diagnoses_all <-c(diagnoses_std, unlist(diagnoses_new$diagnoses) ) %>% unique()

  }else {
    diagnoses_std<- diagnoses %>%
      toupper()
    diagnoses_all <-diagnoses_std
  }

  #To convert ICD10 codes to ICPC - currently users must do this manually
  #because

  if(code_system == "ICD10"){

    stop("Automatic conversion of ICD10 codes to ICPC is not yet supported. Run
icd10_to_icpc() for your required codes and supply these as inputs to curate_kuhr
(via curate_dataset()) with code_system==\"ICPC\")")

    # if(length(diagnoses_std)>0){
    #
    #   diagnoses_std <- icd10_to_icpc( diagnoses_std,
    #                                   recursive=recursive,
    #                                   exclusions=exclusions)
    #
    # }
    #
    # if(exists("diagnoses_new")){
    #
    #   diagnoses_std <- icd10_to_icpc( diagnoses_new,
    #                                   recursive=recursive,
    #                                   exclusions=exclusions)
    # }
    #
    # diagnoses_all <- icd10_to_icpc( diagnoses_all,
    #                                 recursive=recursive,
    #                                 exclusions=exclusions)

  }


  ################################
  ## KUHR processing

  # THree cases: group_all=TRUE, group_all=FALSE, and new_dx (can occur with either of the previous)

  # The consistent portion as a function:

  process_kuhr <- function (d){
    kuhr_reduced_tmp <- suppressWarnings(suppressMessages(kuhrdata %>%
                                                            dplyr::filter( stringr::str_detect(DIAGNOSENE ,d))%>%
                                                            dplyr::rename(LNr = PASIENTLOPENUMMER) %>%
                                                            dplyr::left_join(kuhr_link_moba %>%
                                                                               dplyr::select(LNr, FAAR,dx_owner) %>%
                                                                               dplyr::distinct()) %>%
                                                            dplyr::mutate(date = lubridate::round_date(lubridate::parse_date_time(MNED,"ym"),unit="month")) %>%
                                                            tidyr::separate(MNED, into =c("year","month"), sep = "-") %>%
                                                            dplyr::mutate(childage_at_yrs = as.numeric(year)-FAAR ) %>%
                                                            dplyr::mutate(other_codes= stringr::str_remove_all(DIAGNOSENE, d) ,
                                                                          year=as.numeric(year),
                                                                          month=as.numeric(month))%>%
                                                            dplyr::group_by(LNr) %>%
                                                            dplyr::arrange(year,month) %>%
                                                            dplyr::summarise(received_dx = "yes",
                                                                             received_dx_2x = ifelse(dplyr::n()>1, "yes",NA),
                                                                             n_times_dx = dplyr::n(),
                                                                             date_first_dx = min(date),
                                                                             year_first_dx = min(year),
                                                                             childageyrs_first_dx = min(childage_at_yrs),
                                                                             dates_all_dx = paste0(date, collapse="; "),
                                                                             years_all_dx = paste0(year, collapse="; "),
                                                                             childageyrs_all_dx = paste0(childage_at_yrs, collapse="; "),
                                                                             otherdx_all_dx = paste0(other_codes, collapse="; ")) %>%
                                                            dplyr::ungroup() %>%
                                                            dplyr::mutate(otherdx_all_dx= stringr::str_remove_all(otherdx_all_dx, ",,|,;|;,"))))


    return(kuhr_reduced_tmp)
  }


  #############################

  #Ungrouped dx case (there must be some std diagnoses)

  if(group_all==FALSE&!is.null(diagnoses_std)){
    for(d in diagnoses_std){

      message(paste0(
        "\nProcessing code: ",d,", for variable set number ",match(d,unique(diagnoses_std)), " of ",length(diagnoses_std)+
          if(exists("diagnoses_new")){nrow(diagnoses_new)}else{0} ))

      kuhr_processed_tmp <- process_kuhr(d) %>%
        dplyr::rename_at(dplyr::vars(dplyr::matches("_dx")), ~paste(.,d,"kuhr",sep="_"))

      kuhr_processed <- suppressMessages(dplyr::left_join(kuhr_processed, kuhr_processed_tmp))

    }
  }else {

    #Grouped dx case (there must be some standard diagnoses)

    if(!is.null(diagnoses_std)){

      d <- paste0(diagnoses_std, collapse = "|")

      message(paste0(
        "\nProcessing codes: ",d," as a group because option group_all is set to TRUE.
If you wanted these codes processed individually, re-run with group_all=FALSE.
Resulting variables will be labelled with ", if(!is.null(dx_groupname)){dx_groupname}else{d},"."))

      kuhr_processed_tmp <- process_kuhr(d) %>%
        dplyr::rename_at(dplyr::vars(dplyr::matches("_dx")), ~paste(.,if(!is.null(dx_groupname)){dx_groupname}else{d},"kuhr",sep="_"))

      kuhr_processed <- suppressMessages(dplyr::left_join(kuhr_processed, kuhr_processed_tmp))
    }


  }
  #New dx case (grouping applied by default so no split here)

  if(exists("diagnoses_new")){

    for( dx in unique(diagnoses_new$dx_groupname)){

      d <- paste0(diagnoses_new %>% dplyr::filter(dx_groupname==dx) %>% .$diagnoses, collapse = "|")

      message(paste0(
        "\nProcessing diagnosis codes ",paste0(diagnoses_new %>%
                                            dplyr::filter(dx_groupname==dx) %>% .$diagnoses, collapse = ", ")," as a group; resulting variables will be labelled with ", dx,". Other curate_kuhr options are applied as specified or at their defaults (see ?curate_kuhr)."))

      kuhr_processed_tmp <- process_kuhr(d) %>%
        dplyr::rename_at(dplyr::vars(dplyr::matches("_dx")), ~paste(.,dx,"kuhr",sep="_"))

      kuhr_processed <- suppressMessages(dplyr::left_join(kuhr_processed, kuhr_processed_tmp))

    }
  }

    #Tidy up to preserve RAM before potential reshape (if more than one dx_owner)

    if(exists("kuhr_link_moba")){
      rm(kuhr_link_moba)
    }

    if(mem_preserve>0){
      rm(kuhrdata)
    }


    kuhr_processed <- kuhr_processed %>%
      dplyr::filter(dx_owner %in% dx_owners) %>%
      dplyr::select(-LNr) %>%
      dplyr::mutate(preg_id = as.character(preg_id)) %>%
      dplyr::rename(birth_yr = FAAR)

    if(length(dx_owners)>1){

      kuhr_processed <- suppressMessages(suppressWarnings(kuhr_processed %>%
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

    message("\nKUHR data processing complete.")
    return(kuhr_processed)


}



