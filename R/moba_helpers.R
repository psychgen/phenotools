#' Helper functions for creating MoBa variables with non-standard coding
#'
#' @description
#'
#' Some MoBa variables require bespoke coding. Sometimes this is because
#' of the way responses are coded or because of differences
#' between questionnaire versions. The helper functions within this file allow
#' \code{curate_dataset} to make these variables and include them in
#' datasets alongside standard scale variables and un-processed item retrievals.
#'
#' NB: this is different to retrieval of single MoBa items by item code, as
#' those variables are returned without processing. Here we are dealing with
#' derived variables such as BMI, or scales where the standard coding in
#' \code{curate_moba_scales} isn't capable of handling some idiosyncrasies
#'
#'
#' @param moba_other_vars Df made in \code{curate_dataset}
#' @param moba_other_vars_long Long form of Df made in \code{curate_dataset}
#' @param moba_data Df made in \code{curate_dataset}
#' @param return_items Whether to return an item level dataset
#' @param completion_threshold What proportion of non-missing items in scale
#'
#'
#' @section Non-standard MoBa variable helpers:
#'
#' The MoBa helper functions are listed below along with a short description
#' of the reason why a helper is needed. More info may be available in the notes
#' for a given variables (see notes column in \code{phenotools::moba} or get a
#' dataset report for these).
#'
#' * `single_item()`: items that do not belong to a scale are retrieved by code using this function
#' * `moba_bmi()``: BMI variables must be derived based on weight and height; outliers are removed as part of this process
#' * `moba_mchat()`: some (but not all) M-CHAT variables have item codes that differ between questionnaire versions; a helper is needed to handle this
#' * `moba_nhipic()`: some of the NHIPIC subscales  (extraversion, imagination, neuroticism) have item codes that differ between questionnaire versions; a helper is needed to handle this
#'
#' @importFrom dplyr "%>%"
#' @name moba_helpers
NULL
#> NULL

#' @rdname moba_helpers
#' @export
single_item <- function(moba_other_vars,
                        moba_other_vars_long,
                        moba_data,
                        return_items,
                        completion_threshold){


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

#' @rdname moba_helpers
#' @export
moba_mchat <- function(moba_other_vars,
                       moba_other_vars_long,
                       moba_data,
                       return_items,
                       completion_threshold){

  message("\nProcessing non-standard M-CHAT variables...")

  mchat_scale_data_final <- moba_data %>%
    dplyr::select(preg_id:birth_yr)

  if(return_items==T){
    mchat_item_data_final <- moba_data %>%
      dplyr::select(preg_id:birth_yr)
  }




  #Create a vector of all reversed items among requested vars

  mchat_vars<- dplyr::filter(moba_other_vars, stringr::str_detect(var_name, "mchat"))

  mchat_rvrsd_items <- unlist(strsplit(paste0(dplyr::filter(mchat_vars,!is.na(reversed))$reversed, collapse=","),","))


  # Get MCHAT full items and flag which are alternates to other items

  if(any(unique(moba_other_vars$var_name) == "mchat_full_c_18m")) {

    mchat_vars<-moba_other_vars_long %>%
      dplyr::filter(var_name == "mchat_full_c_18m") %>%
      dplyr::select(var_name,item_no,item_name) %>% #re-coding is assuming items are in the same order as they are entered in the meta-data file - may be safer to do something off of item_name column but not sure how (case_when maybe?)
      dplyr::mutate(item_no= dplyr::recode(item_no, "i1" = "i1", "i2" = "i1", "i3" = "i2", "i4"= "i3", "i5" = "i4", "i6" = "i4", "i7" = "i5",
                                           "i8" = "i6", "i9"= "i7", "i10" = "i7","i11" = "i8", "i12" = "i9", "i13" = "i10", "i14" = "i11",
                                           "i15" = "i12", "i16" = "i13", "i17" = "i14", "i18" = "i15", "i19" = "i16", "i20" = "i16", "i21" = "i17",
                                           "i22" = "i18", "i23" = "i19", "i24" = "i20", "i25" = "i21", "i26" = "i22", "i27" = "i23"))

    mchat_items <- mchat_vars$item_name

    mc_mult <- c("i1", "i4","i7", "i16") #questions that have more than one item code

    mc_rename <- mchat_vars %>% #items for renaming that have more than one item code
      dplyr::filter(c(item_no %in% mc_mult)) %>%
      .$item_name

    # Get responses
    responses_long <- moba_other_vars_long %>%
      dplyr::filter(var_name == "mchat_full_c_18m") %>%
      dplyr::select(tidyselect::matches("response")) %>%
      tidyr::gather(response,val) %>%
      dplyr::distinct() %>%
      tidyr::drop_na(val) %>%
      dplyr::mutate(val=stringr::str_split(val, "\\|")) %>%
      dplyr::mutate(pos_numval = dplyr::row_number()-1,
                    neg_numval = rev(dplyr::row_number()-1)) %>%
      dplyr::select(-response) %>%
      tidyr::unnest(cols=c(val))

    # Get items in long format
    mchat_long_dat <-
      suppressWarnings(suppressMessages(moba_data %>%
      dplyr::select(preg_id,m_id,f_id,BARN_NR,birth_yr, dplyr::all_of(mchat_items)) %>%
      dplyr::mutate_at(dplyr::vars(-preg_id:-birth_yr), list(~haven::as_factor(.) )) %>%
      tidyr::gather(item_name, val, -preg_id:-birth_yr) %>%
      dplyr::left_join(responses_long) %>%
      dplyr::left_join(mchat_vars) %>%
      dplyr::mutate(numval = ifelse(item_name %in% mchat_rvrsd_items, neg_numval,pos_numval))))

    #Calculate scale variable accounting for item alternates

    mchat_scale_data_tmp <-mchat_long_dat %>%
      dplyr::select(-val,-pos_numval,-neg_numval) %>%
      tidyr::unite("item_code", c("item_no","item_name") ) %>%
      tidyr::spread(item_code,numval) %>%
      dplyr::mutate(i1_alt = dplyr::case_when(is.na(i1_EE427) & is.na(i1_EE1005) ~ NA_real_,
                                              is.na(i1_EE427) ~ i1_EE1005,
                                              TRUE ~ i1_EE427),
                    i4_alt = dplyr::case_when(is.na(i4_EE430) & is.na(i4_EE996) ~ NA_real_,
                                              is.na(i4_EE430) ~ i4_EE996,
                                              TRUE ~ i4_EE430),
                    i7_alt = dplyr::case_when(is.na(i7_EE432) & is.na(i7_EE997) ~ NA_real_,
                                              is.na(i7_EE432) ~ i7_EE997,
                                              TRUE ~ i7_EE432),
                    i16_alt = dplyr::case_when(is.na(i16_EE406) & is.na(i16_EE986) ~ NA_real_,
                                               is.na(i16_EE986) ~ i16_EE406,
                                               TRUE ~ i16_EE986)) %>%
      tidyr::gather(item_code, numval, -preg_id:-var_name) %>%
      tidyr::separate(item_code, into =c("item_no","item_name")) %>%
      dplyr::filter(!item_name %in% mc_rename )


    mchat_scale_data_final <-
      suppressMessages(mchat_scale_data_final %>%
                         dplyr::left_join(mchat_scale_data_tmp %>%
                                            dplyr::group_by(preg_id,BARN_NR,var_name) %>%
                                            dplyr::summarise(score = ifelse(sum(!is.na(numval))>=(completion_threshold*dplyr::n()),
                                                                            round(mean(numval, na.rm=T)*dplyr::n(),0),
                                                                            NA)) %>%
                                            tidyr::spread(var_name, score)) )

    # Item level data if requested

    if(return_items==T){

      # Raw variables

      mchat_raw <- mchat_long_dat %>%
        tidyr::unite("var_item_code",c("var_name","item_no","item_name")) %>%
        dplyr::select(-numval,-pos_numval,-neg_numval) %>%
        tidyr::spread(var_item_code, val) %>%
        dplyr::rename_at(dplyr::vars(dplyr::matches("EE")), ~paste0(.x,"_raw"))

      # Processed variables

      mchat_coded <- mchat_scale_data_tmp  %>%
        tidyr::unite("var_item_code",c("var_name","item_no","item_name")) %>%
        tidyr::spread(var_item_code, numval) %>%
        dplyr::rename_at(dplyr::vars(dplyr::matches("EE|_alt")), ~paste0(.x,"_coded"))

      # Join

      mchat_item_data_final <-
        suppressMessages(mchat_item_data_final %>%
        dplyr::left_join(mchat_raw) %>%
        dplyr::left_join(mchat_coded))

      mchat_scale_data_final <-
        suppressMessages(mchat_scale_data_final %>%
                           dplyr::left_join(mchat_item_data_final))
    }



  }

  # Get MCHAT crit items and flag which are alternates to other items

  if(any(unique(moba_other_vars$var_name) == "mchat_crit_c_18m")) {

    mchat_vars<-moba_other_vars_long %>%
      dplyr::filter(var_name == "mchat_crit_c_18m") %>%
      dplyr::select(var_name,item_no,item_name) %>% #re-coding is assuming items are in the same order as they are entered in the meta-data file - may be safer to do something off of item_name column but not sure how (case_when maybe?)
      dplyr::mutate(item_no= dplyr::recode(item_no, "i1" = "i1", "i2" = "i2", "i3" = "i2", "i4"= "i4", "i5" = "i5", "i6" = "i6"))

    mchat_items <- mchat_vars$item_name

    mc_mult <- c("i2") #questions that have more than one item code

    mc_rename <- mchat_vars %>% #items for renaming that have more than one item code
      dplyr::filter(c(item_no %in% mc_mult)) %>%
      .$item_name

    # Get responses
    responses_long <- moba_other_vars_long %>%
      dplyr::filter(var_name == "mchat_crit_c_18m") %>%
      dplyr::select(tidyselect::matches("response")) %>%
      tidyr::gather(response,val) %>%
      dplyr::distinct() %>%
      tidyr::drop_na(val) %>%
      dplyr::mutate(val=stringr::str_split(val, "\\|")) %>%
      dplyr::mutate(pos_numval = dplyr::row_number()-1,
                    neg_numval = rev(dplyr::row_number()-1)) %>%
      dplyr::select(-response) %>%
      tidyr::unnest(cols=c(val))

    # Get items in long format
    mchat_long_dat <-
      suppressMessages(suppressWarnings(moba_data %>%
      dplyr::select(preg_id,m_id,f_id,BARN_NR,birth_yr, dplyr::all_of(mchat_items)) %>%
      dplyr::mutate_at(dplyr::vars(-preg_id:-birth_yr), list(~haven::as_factor(.) )) %>%
      tidyr::gather(item_name, val, -preg_id:-birth_yr) %>%
      dplyr::left_join(responses_long) %>%
      dplyr::left_join(mchat_vars) %>%
      dplyr::mutate(numval = ifelse(item_name %in% mchat_rvrsd_items, neg_numval,pos_numval))))

    #Calculate scale variable accounting for item alternates


    mchat_scale_data_tmp <-mchat_long_dat %>%
      dplyr::select(-val,-pos_numval,-neg_numval) %>%
      tidyr::unite("item_code", c("item_no","item_name") ) %>%
      tidyr::spread(item_code,numval) %>%
      dplyr::mutate(i2_alt = dplyr::case_when(is.na(i2_EE432) & is.na(i2_EE997) ~ NA_real_,
                                              is.na(i2_EE432) ~ i2_EE997,
                                              TRUE ~ i2_EE432)) %>%
      tidyr::gather(item_code, numval, -preg_id:-var_name) %>%
      tidyr::separate(item_code, into =c("item_no","item_name")) %>%
      dplyr::filter(!item_name %in% mc_rename )


    mchat_scale_data_final <-
      suppressMessages(mchat_scale_data_final %>%
      dplyr::left_join(mchat_scale_data_tmp %>%
                         dplyr::group_by(preg_id,BARN_NR,var_name) %>%
                         dplyr::summarise(score = ifelse(sum(!is.na(numval))>=(completion_threshold*dplyr::n()),
                                                         round(mean(numval, na.rm=T)*dplyr::n(),0),
                                                         NA)) %>%
                         tidyr::spread(var_name, score)))

    # Item level data if requested

    if(return_items==T){

      # Raw variables

      mchat_raw <- mchat_long_dat %>%
        tidyr::unite("var_item_code",c("var_name","item_no","item_name")) %>%
        dplyr::select(-numval,-pos_numval,-neg_numval) %>%
        tidyr::spread(var_item_code, val) %>%
        dplyr::rename_at(dplyr::vars(dplyr::matches("EE")), ~paste0(.x,"_raw"))

      # Processed variables

      mchat_coded <- mchat_scale_data_tmp  %>%
        tidyr::unite("var_item_code",c("var_name","item_no","item_name")) %>%
        tidyr::spread(var_item_code, numval) %>%
        dplyr::rename_at(dplyr::vars(dplyr::matches("EE")), ~paste0(.x,"_coded"))

      # Join

      mchat_item_data_final <-
        suppressMessages(mchat_item_data_final %>%
        dplyr::left_join(mchat_raw) %>%
        dplyr::left_join(mchat_coded))


      mchat_scale_data_final <-
        suppressMessages(mchat_scale_data_final %>%
                           dplyr::left_join(mchat_item_data_final))
    }


  }





    return(mchat_scale_data_final)

  message("\nProcessing of non-standard M-CHAT variables complete.")

}

#' @rdname moba_helpers
#' @export

moba_bmi <- function(moba_other_vars,
                     moba_other_vars_long,
                     moba_data,
                     return_items,
                     completion_threshold){

  # Create lookup table with heights and weights defined for each bmi variable

  bmi_vars <-moba_other_vars_long %>%
    dplyr::filter(measure=="BMI") %>%
    dplyr::select(var_name,item_no,item_name) %>%
    dplyr::mutate(item_no= dplyr::recode(item_no, "i1" = "weightkg", "i2" = "heightcm")) %>%
    dplyr::mutate(item_no= dplyr::case_when(item_no=="i3" ~ "weightkg",
                                            item_no=="i4" ~ "heightcm",
                                            TRUE ~ item_no))
  moba_bmi_data_temp <-
    suppressMessages(
      suppressWarnings(
        moba_data %>%
          dplyr::select(preg_id,m_id,f_id,BARN_NR,birth_yr,
                        bmi_vars$item_name) %>%
          tidyr::gather(item_name, value, -preg_id:-birth_yr) %>%
          dplyr::left_join(bmi_vars %>%
                             dplyr::select(item_name, "item_type" =item_no, var_name))))

  moba_bmi_data <- moba_data %>%
    dplyr::select(preg_id:birth_yr)

  for(v in unique(bmi_vars$var_name)){

    message(paste0("Processing BMI variable ",match(v,unique(bmi_vars$var_name))," of ", length(unique(bmi_vars$var_name)),"..." ))


    moba_bmi_data_temp_mini <- moba_bmi_data_temp %>%
      dplyr::filter(var_name == v) %>%
      dplyr::select(preg_id:birth_yr, item_type, value) %>%
      dplyr::group_by(preg_id,BARN_NR, item_type) %>%
      tidyr::fill("value", .direction="downup") %>%
      dplyr::distinct() %>%
      tidyr::spread(item_type,value) %>%
      dplyr::mutate_at(dplyr::vars(weightkg,heightcm), .funs=list(mean = ~mean(.,na.rm=T), sd = ~sd(.,na.rm=T))) %>%
      dplyr::mutate(weight_outlier = ifelse(weightkg>(weightkg_mean+(3*weightkg_sd))|weightkg<(weightkg_mean-(3*weightkg_sd)),"Outliers: weight (+/- 3SDs)",NA),
                    height_outlier = ifelse(heightcm>(heightcm_mean+(3*heightcm_sd))|heightcm<(heightcm_mean-(3*heightcm_sd)),"Outliers: height (+/- 3SDs)",NA))%>%
      dplyr::filter(is.na(weight_outlier), is.na(height_outlier)) %>%
      dplyr::mutate(bmi = weightkg/((heightcm/100)^2)) %>%
      dplyr::mutate_at(dplyr::vars(bmi), .funs=list(bmi_mean = ~mean(.,na.rm=T), bmi_sd = ~sd(.,na.rm=T))) %>%
      dplyr::mutate(bmi_outliers_flag = ifelse(bmi>(bmi_mean+(3*bmi_sd))|bmi<(bmi_mean-(3*bmi_sd)),"Outlier (+/- 3SDs)",NA)) %>%
      dplyr::select(preg_id:birth_yr, weightkg,heightcm, bmi) %>%
      tidyr::gather(valtype, val, -preg_id:-birth_yr) %>%
      dplyr::mutate(var = stringr::str_remove(v,"bmi_")) %>%
      tidyr::unite("varname",c("valtype","var")) %>%
      tidyr::spread(varname,val)

    suppressMessages(suppressWarnings(moba_bmi_data <- moba_bmi_data %>%
                                        dplyr::left_join(moba_bmi_data_temp_mini)))

    }


  ##Re-code Q7 child height (actually in metres) and re-alculate BMI

  if(any(unique(bmi_vars$var_name) == "c_q7yr")){
    moba_bmi_data <- moba_bmi_data %>%
      dplyr::mutate(heightcm_derived_c_7yr = heightcm_derived_c_7yr*100) %>%
      dplyr::mutate(bmi_derived_c_7yr = weightkg_derived_c_7yr/((heightcm_derived_c_7yr/100)^2))
  }
  message("\nProcessing of BMI variables is complete.")
  return(moba_bmi_data)
}

#' @rdname moba_helpers
#' @export
moba_lthmdd <- function(moba_other_vars,
                       moba_other_vars_long,
                       moba_data,
                       return_items,
                       completion_threshold){

  message("\nProcessing LTH-MDD variables...")

  lthmdd_scale_data_final <- moba_data %>%
    dplyr::select(preg_id:birth_yr)

  if(return_items==T){
    lthmdd_item_data_final <- moba_data %>%
      dplyr::select(preg_id:birth_yr)
  }


  #Create a vector of all reversed items among requested vars

  lthmdd_vars<- dplyr::filter(moba_other_vars, stringr::str_detect(var_name, "lthmdd"))

  lthmdd_rvrsd_items <- unlist(strsplit(paste0(dplyr::filter(lthmdd_vars,!is.na(reversed))$reversed, collapse=","),","))



  if(return_items==T){
    mchat_item_scale_data_final <-
    suppressMessages(mchat_scale_data_final %>%
                       dplyr::left_join(mchat_item_data_final))
    return(mchat_item_scale_data_final)
  }else{
    return(mchat_scale_data_final)}

  message("\nProcessing of LTH-MDD variables complete.")

}

#' @rdname moba_helpers
#' @export

moba_nhipic <- function(moba_other_vars,
                       moba_other_vars_long,
                       moba_data,
                       return_items,
                       completion_threshold){

  message("\nProcessing non-standard NHIPIC variables...")

  nhipic_scale_data_final <- moba_data %>%
    dplyr::select(preg_id:birth_yr)

  if(return_items==T){
    nhipic_item_data_final <- moba_data %>%
      dplyr::select(preg_id:birth_yr)
  }

  #Create a vector of all reversed items among requested vars

  nhipic_vars<- dplyr::filter(moba_other_vars, stringr::str_detect(var_name, "nhipic"))

  nhipic_rvrsd_items <- unlist(strsplit(paste0(dplyr::filter(nhipic_vars,!is.na(reversed))$reversed, collapse=","),","))


  # Get NHIPIC extra items and flag which are alternates to other items

  if(any(unique(moba_other_vars$var_name) == "nhipic_extra_c_8yr")) {

    nhipic_vars<-moba_other_vars_long %>%
      dplyr::filter(var_name == "nhipic_extra_c_8yr") %>%
      dplyr::select(var_name,item_no,item_name) %>% #re-coding is assuming items are in the same order as they are entered in the meta-data file - may be safer to do something off of item_name column but not sure how (case_when maybe?)
      dplyr::mutate(item_no= dplyr::recode(item_no, "i1" = "i1", "i2" = "i1", "i3" = "i2", "i4"= "i2", "i5" = "i3", "i6" = "i4", "i7" = "i5",
                                           "i8" = "i6" ))

    nhipic_items <- nhipic_vars$item_name

    mc_mult <- c("i1", "i2") #questions that have more than one item code

    mc_rename <- nhipic_vars %>% #items for renaming that have more than one item code
      dplyr::filter(c(item_no %in% mc_mult)) %>%
      .$item_name

    # Get responses
    responses_long <- moba_other_vars_long %>%
      dplyr::filter(var_name == "nhipic_extra_c_8yr") %>%
      dplyr::select(tidyselect::matches("response")) %>%
      tidyr::gather(response,val) %>%
      dplyr::distinct() %>%
      tidyr::drop_na(val) %>%
      dplyr::mutate(val=stringr::str_split(val, "\\|")) %>%
      dplyr::mutate(pos_numval = dplyr::row_number()-1,
                    neg_numval = rev(dplyr::row_number()-1)) %>%
      dplyr::select(-response) %>%
      tidyr::unnest(cols=c(val))

    # Get items in long format
    nhipic_long_dat <-
      suppressWarnings(suppressMessages(moba_data %>%
                                          dplyr::select(preg_id,m_id,f_id,BARN_NR,birth_yr, dplyr::all_of(nhipic_items)) %>%
                                          dplyr::mutate_at(dplyr::vars(-preg_id:-birth_yr), list(~haven::as_factor(.) )) %>%
                                          tidyr::gather(item_name, val, -preg_id:-birth_yr) %>%
                                          dplyr::left_join(responses_long) %>%
                                          dplyr::left_join(nhipic_vars) %>%
                                          dplyr::mutate(numval = ifelse(item_name %in% nhipic_rvrsd_items, neg_numval,pos_numval))))

    #Calculate scale variable accounting for item alternates

    nhipic_scale_data_tmp <-nhipic_long_dat %>%
      dplyr::select(-val,-pos_numval,-neg_numval) %>%
      tidyr::unite("item_code", c("item_no","item_name") ) %>%
      tidyr::spread(item_code,numval) %>%
      dplyr::mutate(i1_alt = dplyr::case_when(is.na(i1_NN88) & is.na(i1_NN370) ~ NA_real_,
                                              is.na(i1_NN88) ~ i1_NN370,
                                              TRUE ~ i1_NN88),
                    i2_alt = dplyr::case_when(is.na(i2_NN90) & is.na(i2_NN371) ~ NA_real_,
                                              is.na(i2_NN90) ~ i2_NN371,
                                              TRUE ~ i2_NN90)) %>%
      tidyr::gather(item_code, numval, -preg_id:-var_name) %>%
      tidyr::separate(item_code, into =c("item_no","item_name")) %>%
      dplyr::filter(!item_name %in% mc_rename )


    nhipic_scale_data_final <-
      suppressMessages(nhipic_scale_data_final %>%
                         dplyr::left_join(nhipic_scale_data_tmp %>%
                                            dplyr::group_by(preg_id,BARN_NR,var_name) %>%
                                            dplyr::summarise(score = ifelse(sum(!is.na(numval))>=(completion_threshold*dplyr::n()),
                                                                            round(mean(numval, na.rm=T)*dplyr::n(),0),
                                                                            NA)) %>%
                                            tidyr::spread(var_name, score)) )

    # Item level data if requested

    if(return_items==T){

      # Raw variables

      nhipic_raw <- nhipic_long_dat %>%
        tidyr::unite("var_item_code",c("var_name","item_no","item_name")) %>%
        dplyr::select(-numval,-pos_numval,-neg_numval) %>%
        tidyr::spread(var_item_code, val) %>%
        dplyr::rename_at(dplyr::vars(dplyr::matches("NN")), ~paste0(.x,"_raw"))

      # Processed variables

      nhipic_coded <- nhipic_scale_data_tmp  %>%
        tidyr::unite("var_item_code",c("var_name","item_no","item_name")) %>%
        tidyr::spread(var_item_code, numval) %>%
        dplyr::rename_at(dplyr::vars(dplyr::matches("NN|_alt")), ~paste0(.x,"_coded"))

      # Join

      nhipic_item_data_final <-
        suppressMessages(nhipic_item_data_final %>%
                           dplyr::left_join(nhipic_raw) %>%
                           dplyr::left_join(nhipic_coded))

      nhipic_scale_data_final <-
        suppressMessages(nhipic_scale_data_final %>%
                           dplyr::left_join(nhipic_item_data_final))
    }



  }


  # Get NHIPIC imag items and flag which are alternates to other items

  if(any(unique(moba_other_vars$var_name) == "nhipic_imag_c_8yr")) {

    nhipic_vars<-moba_other_vars_long %>%
      dplyr::filter(var_name == "nhipic_imag_c_8yr") %>%
      dplyr::select(var_name,item_no,item_name) %>% #re-coding is assuming items are in the same order as they are entered in the meta-data file - may be safer to do something off of item_name column but not sure how (case_when maybe?)
      dplyr::mutate(item_no= dplyr::recode(item_no, "i1" = "i1", "i2" = "i1", "i3" = "i2", "i4"= "i3", "i5" = "i4", "i6" = "i5", "i7" = "i6" ))

    nhipic_items <- nhipic_vars$item_name

    mc_mult <- c("i1") #questions that have more than one item code

    mc_rename <- nhipic_vars %>% #items for renaming that have more than one item code
      dplyr::filter(c(item_no %in% mc_mult)) %>%
      .$item_name

    # Get responses
    responses_long <- moba_other_vars_long %>%
      dplyr::filter(var_name == "nhipic_imag_c_8yr") %>%
      dplyr::select(tidyselect::matches("response")) %>%
      tidyr::gather(response,val) %>%
      dplyr::distinct() %>%
      tidyr::drop_na(val) %>%
      dplyr::mutate(val=stringr::str_split(val, "\\|")) %>%
      dplyr::mutate(pos_numval = dplyr::row_number()-1,
                    neg_numval = rev(dplyr::row_number()-1)) %>%
      dplyr::select(-response) %>%
      tidyr::unnest(cols=c(val))

    # Get items in long format
    nhipic_long_dat <-
      suppressWarnings(suppressMessages(moba_data %>%
                                          dplyr::select(preg_id,m_id,f_id,BARN_NR,birth_yr, dplyr::all_of(nhipic_items)) %>%
                                          dplyr::mutate_at(dplyr::vars(-preg_id:-birth_yr), list(~haven::as_factor(.) )) %>%
                                          tidyr::gather(item_name, val, -preg_id:-birth_yr) %>%
                                          dplyr::left_join(responses_long) %>%
                                          dplyr::left_join(nhipic_vars) %>%
                                          dplyr::mutate(numval = ifelse(item_name %in% nhipic_rvrsd_items, neg_numval,pos_numval))))

    #Calculate scale variable accounting for item alternates

    nhipic_scale_data_tmp <-nhipic_long_dat %>%
      dplyr::select(-val,-pos_numval,-neg_numval) %>%
      tidyr::unite("item_code", c("item_no","item_name") ) %>%
      tidyr::spread(item_code,numval) %>%
      dplyr::mutate(i1_alt = dplyr::case_when(is.na(i1_NN82) & is.na(i1_NN369) ~ NA_real_,
                                              is.na(i1_NN82) ~ i1_NN369,
                                              TRUE ~ i1_NN82)) %>%
      tidyr::gather(item_code, numval, -preg_id:-var_name) %>%
      tidyr::separate(item_code, into =c("item_no","item_name")) %>%
      dplyr::filter(!item_name %in% mc_rename )


    nhipic_scale_data_final <-
      suppressMessages(nhipic_scale_data_final %>%
                         dplyr::left_join(nhipic_scale_data_tmp %>%
                                            dplyr::group_by(preg_id,BARN_NR,var_name) %>%
                                            dplyr::summarise(score = ifelse(sum(!is.na(numval))>=(completion_threshold*dplyr::n()),
                                                                            round(mean(numval, na.rm=T)*dplyr::n(),0),
                                                                            NA)) %>%
                                            tidyr::spread(var_name, score)) )

    # Item level data if requested

    if(return_items==T){

      # Raw variables

      nhipic_raw <- nhipic_long_dat %>%
        tidyr::unite("var_item_code",c("var_name","item_no","item_name")) %>%
        dplyr::select(-numval,-pos_numval,-neg_numval) %>%
        tidyr::spread(var_item_code, val) %>%
        dplyr::rename_at(dplyr::vars(dplyr::matches("NN")), ~paste0(.x,"_raw"))

      # Processed variables

      nhipic_coded <- nhipic_scale_data_tmp  %>%
        tidyr::unite("var_item_code",c("var_name","item_no","item_name")) %>%
        tidyr::spread(var_item_code, numval) %>%
        dplyr::rename_at(dplyr::vars(dplyr::matches("NN|_alt")), ~paste0(.x,"_coded"))

      # Join

      nhipic_item_data_final <-
        suppressMessages(nhipic_item_data_final %>%
                           dplyr::left_join(nhipic_raw) %>%
                           dplyr::left_join(nhipic_coded))

      nhipic_scale_data_final <-
        suppressMessages(nhipic_scale_data_final %>%
                           dplyr::left_join(nhipic_item_data_final))
    }



  }


  # Get NHIPIC neurot items and flag which are alternates to other items

  if(any(unique(moba_other_vars$var_name) == "nhipic_neurot_c_8yr")) {

    nhipic_vars<-moba_other_vars_long %>%
      dplyr::filter(var_name == "nhipic_neurot_c_8yr") %>%
      dplyr::select(var_name,item_no,item_name) %>% #re-coding is assuming items are in the same order as they are entered in the meta-data file - may be safer to do something off of item_name column but not sure how (case_when maybe?)
      dplyr::mutate(item_no= dplyr::recode(item_no, "i1" = "i1", "i2" = "i1", "i3" = "i2", "i4"= "i3", "i5" = "i4", "i6" = "i5", "i7" = "i6",
                                           "i8" = "i6" ))

    nhipic_items <- nhipic_vars$item_name

    mc_mult <- c("i1", "i6") #questions that have more than one item code

    mc_rename <- nhipic_vars %>% #items for renaming that have more than one item code
      dplyr::filter(c(item_no %in% mc_mult)) %>%
      .$item_name

    # Get responses
    responses_long <- moba_other_vars_long %>%
      dplyr::filter(var_name == "nhipic_neurot_c_8yr") %>%
      dplyr::select(tidyselect::matches("response")) %>%
      tidyr::gather(response,val) %>%
      dplyr::distinct() %>%
      tidyr::drop_na(val) %>%
      dplyr::mutate(val=stringr::str_split(val, "\\|")) %>%
      dplyr::mutate(pos_numval = dplyr::row_number()-1,
                    neg_numval = rev(dplyr::row_number()-1)) %>%
      dplyr::select(-response) %>%
      tidyr::unnest(cols=c(val))

    # Get items in long format
    nhipic_long_dat <-
      suppressWarnings(suppressMessages(moba_data %>%
                                          dplyr::select(preg_id,m_id,f_id,BARN_NR,birth_yr, dplyr::all_of(nhipic_items)) %>%
                                          dplyr::mutate_at(dplyr::vars(-preg_id:-birth_yr), list(~haven::as_factor(.) )) %>%
                                          tidyr::gather(item_name, val, -preg_id:-birth_yr) %>%
                                          dplyr::left_join(responses_long) %>%
                                          dplyr::left_join(nhipic_vars) %>%
                                          dplyr::mutate(numval = ifelse(item_name %in% nhipic_rvrsd_items, neg_numval,pos_numval))))

    #Calculate scale variable accounting for item alternates

    nhipic_scale_data_tmp <-nhipic_long_dat %>%
      dplyr::select(-val,-pos_numval,-neg_numval) %>%
      tidyr::unite("item_code", c("item_no","item_name") ) %>%
      tidyr::spread(item_code,numval) %>%
      dplyr::mutate(i1_alt = dplyr::case_when(is.na(i1_NN81) & is.na(i1_NN368) ~ NA_real_,
                                              is.na(i1_NN81) ~ i1_NN368,
                                              TRUE ~ i1_NN81),
                    i6_alt = dplyr::case_when(is.na(i6_NN107) & is.na(i6_NN372) ~ NA_real_,
                                              is.na(i6_NN107) ~ i6_NN372,
                                              TRUE ~ i6_NN107)) %>%
      tidyr::gather(item_code, numval, -preg_id:-var_name) %>%
      tidyr::separate(item_code, into =c("item_no","item_name")) %>%
      dplyr::filter(!item_name %in% mc_rename )


    nhipic_scale_data_final <-
      suppressMessages(nhipic_scale_data_final %>%
                         dplyr::left_join(nhipic_scale_data_tmp %>%
                                            dplyr::group_by(preg_id,BARN_NR,var_name) %>%
                                            dplyr::summarise(score = ifelse(sum(!is.na(numval))>=(completion_threshold*dplyr::n()),
                                                                            round(mean(numval, na.rm=T)*dplyr::n(),0),
                                                                            NA)) %>%
                                            tidyr::spread(var_name, score)) )

    # Item level data if requested

    if(return_items==T){

      # Raw variables

      nhipic_raw <- nhipic_long_dat %>%
        tidyr::unite("var_item_code",c("var_name","item_no","item_name")) %>%
        dplyr::select(-numval,-pos_numval,-neg_numval) %>%
        tidyr::spread(var_item_code, val) %>%
        dplyr::rename_at(dplyr::vars(dplyr::matches("NN")), ~paste0(.x,"_raw"))

      # Processed variables

      nhipic_coded <- nhipic_scale_data_tmp  %>%
        tidyr::unite("var_item_code",c("var_name","item_no","item_name")) %>%
        tidyr::spread(var_item_code, numval) %>%
        dplyr::rename_at(dplyr::vars(dplyr::matches("NN|_alt")), ~paste0(.x,"_coded"))

      # Join

      nhipic_item_data_final <-
        suppressMessages(nhipic_item_data_final %>%
                           dplyr::left_join(nhipic_raw) %>%
                           dplyr::left_join(nhipic_coded))

      nhipic_scale_data_final <-
        suppressMessages(nhipic_scale_data_final %>%
                           dplyr::left_join(nhipic_item_data_final))
    }



  }



  return(nhipic_scale_data_final)

  message("\nProcessing of non-standard NHIPIC variables complete.")

}






