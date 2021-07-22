#' Simulate dataset
#'
#' \code{sim_dataset} returns a dataframe with simulated
#' data suitable for sharing with analysis code
#'
#' Detailed description...
#'
#' @param dataset A data.frame with variables to be simulated
#' @export
#' @importFrom dplyr "%>%"


sim_data <- function(dataset, ...)
{
simulated <- fakeR::simulate_dataset(dataset, stealth.level = 2, ...)
return(simulated)
}

