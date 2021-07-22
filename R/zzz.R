.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0(
    "#########################################################################
    \nThis is phenotools version ", packageVersion(pkgname),".
    \nFor all verisons <1, quality control of the variables phenotools makes is
ongoing, so you should be alert for problems and prepared to re-run analyses
based on phenotools-curated datasets.\n
##########################################################################"))
}
