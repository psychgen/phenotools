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


#NPR

#Script to make ICD codes list in phenotools format

#Read in raw codes, sourced from https://www.cms.gov/Medicare/Coding/ICD10/2018-ICD-10-CM-and-GEMs
#via https://github.com/k4m1113/ICD-10-CSV

#Update 2024 -- user noted that code F83 was missing despite being in the data;
#this is because the code is missing in these CSV files
#now built in a failsafe to capture all possible codes

raw_cats <- read_csv("./data-raw/categories.csv", col_names = F) %>%
  filter(str_length(X1)==3)
raw_codes <- read_csv("./data-raw/codes.csv", col_names = F)

codes <- raw_codes %>%
  bind_rows(raw_cats) %>%
  distinct() %>%
  add_row(X1= unique(str_sub(raw_codes$X1, end =2)), X2= rep(NA,length(unique(str_sub(raw_codes$X1, end =2))))) %>%
  add_row(X1= unique(str_sub(raw_codes$X1, end =3)) %>%
            .[str_length(.)==3],
          X2= rep(NA,length(unique(str_sub(raw_codes$X1, end =3)) %>%
                              .[str_length(.)==3]))) %>%
  add_row(X1= unique(str_sub(raw_codes$X1, end =4)) %>%
            .[str_length(.)==4],
          X2= rep(NA,length(unique(str_sub(raw_codes$X1, end =4)) %>%
                              .[str_length(.)==4]))) %>%
  mutate(chapter= ifelse(str_length(X1)==2, X1,NA),
         level2= ifelse(str_length(X1)==3, X1,NA),
         descriptor=ifelse(str_length(X1)==3, X2,NA),
         level3= ifelse(str_length(X1)==4, X1,NA),
         descriptor=ifelse(str_length(X1)==4, X2,descriptor),
         level4= ifelse(str_length(X1)==5, X1,NA),
         descriptor=ifelse(str_length(X1)==5, X2,descriptor)) %>%
  select(chapter, level2, level3, level4, descriptor)


#Read in raw section titles, extracted from https://icd.who.int/browse10/2019
titles_lkp <- read_csv("./data-raw/chapters.csv", col_names = F)
#Function for creating the full range of level2 codes in a chapter title
seq_chapt <- function(start,end){
  nums <- c(paste0("0", c(0:9)), 10:99)
  all <- sort(unlist(list(outer(LETTERS, nums, paste0))))
  required <- all[which(all==start):which(all==end)]
  all_chaps <- paste0(unique(str_sub(required,end=3)),collapse=",")
  return(all_chaps)
}
titles_lkp <- titles_lkp %>%
  mutate(across(everything(), str_trim)) %>%
  rowwise() %>%
  mutate(all_chapters = seq_chapt(start=X1,end=X2)) %>%
  select(-X1,-X2) %>%
  mutate(all_chapters = strsplit(all_chapters, ",")) %>%
  unnest(all_chapters) %>%
  mutate(all_chapters=str_sub(all_chapters, end=2)) %>%
  distinct()

 #Add chapter titles
final_codes <- codes %>%
  left_join(titles_lkp %>%
              select("chapter"=all_chapters, "chapt_desc"=X3)) %>%
  mutate(descriptor=ifelse(!is.na(chapt_desc),chapt_desc,descriptor)) %>%
  select(chapter,level2,level3,level4,descriptor)

#Finally make sure we have all possible codes, to ensure compatibility with earlier versions of phenotools
#and handle missing codes from source csv files

nums <- paste0(rep(c(0:9), each=100), c(paste0("0", c(0:9)), 10:99))
all <- sort(unlist(list(outer(LETTERS, nums, paste0))))
all1 <- tibble(chapter=unique(str_sub(all,end=2)),level2=NA,level3=NA,level4=NA,descriptor=NA) %>%
  filter(!chapter %in% final_codes$chapter)
all2 <- tibble(chapter=NA,level2=unique(str_sub(all,end=3)),level3=NA,level4=NA,descriptor=NA) %>%
  filter(!level2 %in% final_codes$level2)
all3 <- tibble(chapter=NA,level2=NA,level3=all,level4=NA,descriptor=NA) %>%
  filter(!level3 %in% final_codes$level3)

npr <- final_codes %>%
  bind_rows(all1) %>%
  bind_rows(all2) %>%
  bind_rows(all3) %>%
  mutate(chapter_all = chapter,
         chapter_all = ifelse(!is.na(level2), str_sub(level2,end=2),chapter_all),
         chapter_all = ifelse(!is.na(level3), str_sub(level3,end=2),chapter_all),
         chapter_all = ifelse(!is.na(level4), str_sub(level4,end=2),chapter_all),
         l2_all = level2,
         l2_all = ifelse(!is.na(level3), str_sub(level3,end=3),l2_all),
         l2_all = ifelse(!is.na(level4), str_sub(level4,end=3),l2_all),
         l3_all = level3,
         l3_all = ifelse(!is.na(level4), str_sub(level4,end=4),l3_all)) %>%
  arrange(chapter_all,chapter,l2_all,level2,l3_all,level3,level4 ) %>%
  group_by(chapter, level2) %>%
  filter(n()!=2|!is.na(descriptor)) %>%
  group_by(chapter, level2, level3) %>%
  filter(n()!=2|!is.na(descriptor)) %>%
  distinct()%>%
  select(chapter,level2,level3,level4,descriptor)

# Add in any missing descriptors from the old files

npr_f <-readxl::read_excel("//ess01/P471/data/durable/common/phenotools_pkg/data/ICD10_F_NPR_list_250321.xlsx",col_names = T)
npr_other <-readxl::read_excel("//ess01/P471/data/durable/common/phenotools_pkg/data/ICD10_other_NPR_list_150322.xlsx",col_names = T)
npr_old <- npr_f %>%
  bind_rows(npr_other) %>%
  select(level3,"descriptor_old"= descriptor) %>%
  drop_na(level3)

npr <- npr %>%
  left_join(npr_old) %>%
  mutate(descriptor=ifelse(is.na(descriptor),descriptor_old,descriptor)) %>%
  select(-descriptor_old) %>%
  ungroup()


# Add to the package
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
                                Far2 = make_moba_filepath("Far2"),
                                Q14yM = make_moba_filepath("Q14yM"),
                                Q14yB = make_moba_filepath("Q14yB")) %>%
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


#p471 variable reference list

p471_npr = read_delim("//ess01/P471/data/durable/data/NPR/NPR_requested_p471_2024.csv")

p471 = p471_npr %>%
  bind_rows(phenotools::npr %>%
              select("var_name"=level3) %>%
              filter(str_detect(var_name,
                                p471_npr %>%
                                  filter(level==3) %>%
                                  .$var_name %>%
                                  paste0(collapse="|")))) %>%
  bind_rows(phenotools::npr %>%
              select("var_name"=level4) %>%
              filter(str_detect(var_name,
                                p471_npr %>%
                                  filter(level==4) %>%
                                  .$var_name %>%
                                  paste0(collapse="|")))) %>%
  arrange(var_name) %>%
  select(var_name) %>%
  bind_rows(phenotools::available_variables(source="moba") %>%
              select(var_name)) %>%
  bind_rows(phenotools::kuhr %>%
              select("var_name"=Code))


#########NB, for MoBa and KUHR, this assumes that p471 has all variables phenotools can make
#########This is true for now, but ideally available_variables should return a canonical list
#########of variables for these sources that is project agnostic - at that point this code will
#########need updating to reflect what p471 actually has available

use_data(p471, overwrite = TRUE)
