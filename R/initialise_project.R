#' Initialise project
#'
#' \code{initialise_project} populates a specified directory
#' with the file structure required to make full use of phenotools
#'
#' Detailed description...
#'
#' @param path Where is the top-level directory for the new
#' project?
#' @param template_filepath Where is the template new project directory?
#' (Defaults to correct location for p471)
#' @export
#' @importFrom dplyr "%>%"

initialise_project <- function(path,
                               template_filepath="N:/data/durable/common/new_project_template/."){
  message("Initialising...")
  # Create the project path given the name chosen by the user:
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  # Copy files from template to new
  file.copy(from=template_filepath, to=path,
            overwrite = FALSE, recursive = TRUE,
            copy.mode = TRUE)

  file.rename(from=paste0(path,"/new_project_template.Rproj"),
              to=paste0(path,"/",
                        stringr::str_split(path,"/")[[1]][length(stringr::str_split(path,"/")[[1]])], ".Rproj"))

   message("\nProject initialised successfully. Open via Rstudio>File>Open project...
or open the >yourproject<.Rproj file from Windows Explorer.")
}



