library(readxl)
library(tidyverse)
library(usethis)

# Read in from MoBa spreadsheet and filter only complete entries

moba <- read_excel("//ess01/P471/data/durable/common/phenotools_pkg/data/moba_pheno_metadata_safe.xlsx")%>%
  filter(complete=="y", safe=="y") %>%
  mutate_all( list(~str_remove_all(.,"\""))) %>%
  mutate_at(c("items","consistent","reversed"), ~str_remove_all(.," ")) %>%
  select(-coded_by)

use_data(moba, overwrite=TRUE)

# Read in NPR codes

npr_f <-readxl::read_excel("//ess01/P471/data/durable/common/phenotools_pkg/data/ICD10_F_NPR_list_250321.xlsx",col_names = T)
npr_other <-readxl::read_excel("//ess01/P471/data/durable/common/phenotools_pkg/data/ICD10_other_NPR_list_150322.xlsx",col_names = T)
npr <- npr_f %>%  bind_rows(npr_other)
use_data(npr, overwrite = TRUE)


# Read in KUHR codes

kuhr <-readxl::read_excel("//ess01/P471/data/durable/common/phenotools_pkg/data/icpc_2e_v7.xlsx",col_names = T)

use_data(kuhr, overwrite = TRUE)

# Read all moba questionnaires and make a list of variables names

make_moba_filepath <- function(x,
                               moba_data_root_dir="//ess01/P471/data/durable/data/MoBaPhenoData/PDB2306_MoBa_V12/SPSS/",
                               PDB="2306",
                               moba_data_version = 12,
                               name=NULL){
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
                                Far2 = make_moba_filepath("Far2")) %>%
  t() %>% as.data.frame %>%
  tibble::rownames_to_column() %>%
  `colnames<-`(c("questionnaire","filepath"))

q_names <- list()

for(q in unique(moba_filepaths$questionnaire)){

  message(paste0("\nLoading data from questionnaire ",q,", which is number ",match(q,unique(moba_filepaths$questionnaire))," of ", length(unique(moba_filepaths$questionnaire)) ))

  suppressMessages(qnames_temp <- haven::read_spss(moba_filepaths %>% dplyr::filter(questionnaire==q) %>% .$filepath ) %>%
                       names())

   q_names[[q]] <-qnames_temp
}

moba_varnames <- q_names

use_data(moba_varnames, overwrite = TRUE)
