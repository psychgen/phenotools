#' BMI variable creation helper function
#'
#' \code{moba_bmi} is a helper function
#' called by \code{curate_dataset} to code up BMI variables
#'
#' Detailed description...
#'
#' @param moba_other_vars Df made in \code{curate_dataset}
#' @param moba_other_vars_long Long form of Df made in \code{curate_dataset}
#' @param moba_data Df made in \code{curate_dataset}
#' @export
#' @importFrom dplyr "%>%"

moba_bmi <- function(moba_other_vars,
                     moba_other_vars_long,
                     moba_data){

  # Create lookup table with heights and weights defined for each bmi variable

  bmi_vars <-moba_other_vars_long %>%
    dplyr::filter(measure=="BMI") %>%
    dplyr::select(var_name,item_no,item_name) %>%
    dplyr::mutate(item_no= dplyr::recode(item_no, "i1" = "weightkg", "i2" = "heightcm"))

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

    message(paste0("Processing BMI variable ",match(v,unique(bmi_vars$var_name))," of ", length(unique(bmi_vars$var_name)) ))


    moba_bmi_data_temp_mini <- moba_bmi_data_temp %>%
      dplyr::filter(var_name == v) %>%
      dplyr::select(preg_id:birth_yr, item_type, value) %>%
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
      tidyr::unite("varname",c("var","valtype")) %>%
      tidyr::spread(varname,val)

    suppressMessages(suppressWarnings(moba_bmi_data <- moba_bmi_data %>%
                                        dplyr::left_join(moba_bmi_data_temp_mini)))
  }

  ##Re-code Q7 child height (actually in metres) and re-alculate BMI

  if(any(unique(bmi_vars$var_name) == "bmi_child_q7yr")){
    moba_bmi_data <- moba_bmi_data %>%
      dplyr::mutate(child_q7yr_heightcm = child_q7yr_heightcm*100) %>%
      dplyr::mutate(child_q7yr_bmi = child_q7yr_weightkg/((child_q7yr_heightcm/100)^2))
  }
  message("\nProcessing of BMI variables is complete.")
  return(moba_bmi_data)
}
