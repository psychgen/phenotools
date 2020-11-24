library(readxl)
library(tidyverse)
library(usethis)

# Read in from spreadsheet and filter only complete entries

moba <- read_excel("N:/data/durable/common/moba_pheno_pkg/data/moba_pheno_metadata.xlsx")%>%
  filter(complete=="y") %>%
  mutate_all( list(~str_remove_all(.,"\""))) %>%
  select(-coded_by)

use_data(moba, overwrite=TRUE)
