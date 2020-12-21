library(readxl)
library(tidyverse)
library(usethis)

# Read in from spreadsheet and filter only complete entries

moba <- read_excel("//tsd-evs/p471/data/durable/common/phenotools_pkg/data/moba_pheno_metadata_safe.xlsx")%>%
  filter(complete=="y") %>%
  mutate_all( list(~str_remove_all(.,"\""))) %>%
  select(-coded_by)

use_data(moba, overwrite=TRUE)
