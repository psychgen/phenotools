#' Curate moba scales
#'
#' \code{curate_moba_scales} is is called by \code{curate_dataset}
#' when moba scale variables are requested
#'
#'
#' Detailed description...
#'
#' @param moba_scale_vars Df created in \code{curate_dataset}
#' @param moba_scale_vars_long Long format Df created in \code{curate_dataset}
#' @param moba_data Df created in \code{curate_dataset}
#' @param completion_threshold What proportion of scale items need
#' to be present for a scale score to be computed?
#' @param return_items Output item-level data?
#' @param consistent_items Only use wave-to-wave consistent items
#' for variables that are measured longitudinally?
#' @param transformations Not yet implemented
#' @export
#' @importFrom dplyr "%>%"


curate_moba_scales <- function(moba_scale_vars,
                               moba_scale_vars_long,
                               moba_data,
                               completion_threshold=0.5,
                               return_items=FALSE,
                               consistent_items=FALSE,
                               transformations=NULL){


  # process scale vars - should create progress indicator for this as it can take a while (the below is a v rough approx)
  # need to do this by questinnnaire or phenotype to avoid massive dataset reshapes


  message(paste0("\nProcessing MoBa scale variables. These are: \n\n",paste0(c(moba_scale_vars$var_name), collapse="", sep="\n"),
                 "\nExpect a wait of up to:\n\n",round(1.191681*ncol(moba_data),1), " seconds (",
                 round((1.191681*ncol(moba_data))/60,1)," mins)\n
for your requested scales..." ))

  moba_scale_data <- moba_data %>%
    dplyr::select(preg_id:birth_yr)
  if(return_items==T){
    moba_item_data <- moba_data%>%
      dplyr::select(preg_id:birth_yr)
      }

  #Create a vector of all reversed items among requested vars
  moba_rvrsd_items <- unlist(strsplit(paste0(dplyr::filter(moba_scale_vars,!is.na(reversed))$reversed, collapse=","),","))


  for(v in unique(moba_scale_vars_long$var_name)){

    responses_long <- moba_scale_vars %>%
      dplyr::filter(var_name == v) %>%
      dplyr::select(matches("response")) %>%
      tidyr::gather(response,val) %>%
      tidyr::drop_na(val) %>%
      dplyr::mutate(pos_numval = dplyr::row_number()-1,
                    neg_numval = rev(dplyr::row_number()-1)) %>%
      dplyr::select(-response)

    moba_item_data_temp <-
      suppressMessages(
        suppressWarnings(
          moba_data %>%
            dplyr::select(preg_id,m_id,f_id,BARN_NR,birth_yr,unlist(strsplit(paste0(dplyr::filter(moba_scale_vars_long,var_name == v)$item_name, collapse=","),","))) %>%
            dplyr::mutate_at(dplyr::vars(-preg_id:-birth_yr), list(~haven::as_factor(.) )) %>%
            tidyr::gather(item_name, val, -preg_id:-birth_yr) %>%
            dplyr::left_join(responses_long))) %>%
      dplyr::mutate(numval = ifelse(item_name %in% moba_rvrsd_items, neg_numval,pos_numval))


    moba_scale_data_temp <- moba_item_data_temp %>%
      dplyr::group_by(preg_id,BARN_NR,var_name) %>%
      dplyr::summarise(score = ifelse(sum(!is.na(numval))>=(completion_threshold*n()),
                                      round(mean(numval, na.rm=T)*n(),0),
                                      NA)) %>%
      tidyr::spread(var_name, score)

    moba_scale_data <-
      suppressMessages(
        suppressWarnings(
          moba_scale_data %>%
            dplyr::left_join(moba_scale_data_temp)))

    ##Create raw/processed item dataset if return_items ==true
    if(return_items==T){
      moba_item_raw_temp <-
        suppressMessages(
          suppressWarnings(
            moba_item_data_temp %>%
              dplyr::select(-tidyselect::matches("response")) %>%
              dplyr::left_join(moba_scale_vars_long %>%
                                 dplyr::filter(var_name == v) %>%
                                 dplyr::select(item_name, var_name, item_no) %>%
                                 tidyr::unite("var_item", c("var_name","item_no"))))) %>%
        tidyr::unite("var_item_code",c("var_item","item_name")) %>%
        dplyr::select(-numval,-var_name) %>%
        tidyr::spread(var_item_code, val)%>%
        dplyr::rename_at(dplyr::vars(-preg_id:-birth_yr), function(x){paste0(x,"_raw")} )
      moba_item_coded_temp <-
        suppressMessages(
          suppressWarnings(
            moba_item_data_temp %>%
              dplyr::select(-tidyselect::matches("response")) %>%
              dplyr::left_join(moba_scale_vars_long %>%
                                 dplyr::filter(var_name == v) %>%
                                 dplyr::select(item_name, var_name, item_no) %>%
                                 tidyr::unite("var_item", c("var_name","item_no"))))) %>%
        tidyr::unite("var_item_code",c("var_item","item_name")) %>%
        dplyr::select(-val,-var_name) %>%
        tidyr::spread(var_item_code, numval) %>%
        dplyr::rename_at(dplyr::vars(-preg_id:-birth_yr), function(x){paste0(x,"_coded")} )

      moba_item_data_temp <-
        suppressMessages(
          suppressWarnings(
            moba_item_raw_temp %>%
              dplyr::left_join(moba_item_coded_temp)))

      moba_item_data <-
        suppressMessages(
          suppressWarnings(
            moba_item_data %>%
              dplyr::left_join(moba_item_data_temp)))

    }

  }

message("\nProcessing of MoBa scale variables complete.")

if(return_items==T){
    return(list(scales = moba_scale_data, items = moba_item_data))
  }else{
    return(moba_scale_data)}


}

