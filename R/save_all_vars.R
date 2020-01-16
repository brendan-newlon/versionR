#' save_all_vars
#'
#' Save all objects in the Global Environment
#'
#' @return Backups of all variables currently in the Global Environment are saved as .rds files within a subdirectory of your working directory at ../Env/Versions/.
#'
#' @export
#'
#' @examples
#' save_all_vars()
#'
save_all_vars <- function(envir = .GlobalEnv) {
  # create save directories
  dir.create(file.path("Env"), showWarnings = FALSE)
  dir.create(file.path("Env", "Versions"), showWarnings = FALSE)
  dir.create(file.path("Env", "Versions", "save_all_vars"),
             showWarnings = FALSE)
  vars <- ls(envir)
  vars <-
    append(vars, ls()) # second time to include the "vars" obj as a list we'll use to restore later
  for (i in 1:length(vars)) {
    saveRDS(get(vars[i]), paste(file.path(
      "Env",
      "Versions",
      "save_all_vars",
      paste0(vars[i],
             "_", gsub("[: -]", "", Sys.time(), perl = TRUE), ".rds")
    )))
  }
}
# create data files to back up all R objects in the Global Environment
# Use: save_all_vars()
