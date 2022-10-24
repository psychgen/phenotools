
#' Pre-load KUHR data
#'
#' \code{preload_nor} returns a dataframe with the full KUHR dataset, useful if
#' you want to run curate_dataset multiple times for different KUHR variables
#' without waiting for the KUHR load each time; use in conjunction with the
#' 'kuhr_full' option in curate_kuhr (passed on by curate_dataset).
#'
#'
#' Detailed description...
#'
#' @param kuhr_data_root_dir Where is the raw KUHR phenotypic data? (default
#' is for p471)
#' @param kuhr_filenames_override What is the name of the KUHR file? (default
#' is for p471)
#' @param primary_care_only can only be TRUE at present
#' @export
#' @importFrom dplyr "%>%"
#' @importFrom lubridate "%--%"


preload_kuhr <- function(kuhr_data_root_dir="//ess01/P471/data/durable/data/KUHR/processed/",
                         kuhr_filenames_override = NULL,
                         primary_care_only ="TRUE")
{

  message(
    "\n\nReading in KUHR data file (this is a large file so expect a wait of >2 mins)...")

  if(!is.null(kuhr_filenames_override)){
    kuhrfile_list <- kuhr_filenames_override
  }else{
    kuhrfile_list <- list.files(kuhr_data_root_dir)[stringr::str_detect(list.files(kuhr_data_root_dir), "Data_KUHR.csv")]
  }

  for(kuhrfile in kuhrfile_list){


    message(paste0("Reading KUHR data from year: ", stringr::str_sub(kuhrfile,end=4),"..."))
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


  colnames(kuhrdata) <- iconv(names(kuhrdata),"UTF-8", "UTF-8",sub='')



  message(
    "\nKUHR pre-load complete. You can now run curate_dataset with the object in which
this KUHR data is held listed under the 'kuhr_full' argument (or 'kuhr_preprocessed'
if you have preloaded pre-processed data - see ?curate_kuhr for requierments).")

  return(kuhr_full)
}
