#' versionR
#'
#' Helpful R functions for version control of R data objects.
#'
#' @docType package
#' @name versionR
#'
#'
#' @import(magrittr)
#' @import(stringr)
#' @import(tools)
#' @import(tidyr)




#' saveVersion
#'
#' save an object by name (in quotes).
#'
#' @param x The name of an R object in quotes
#'
#' @return The R object is saved as a .rds file within a subdirectory of your working directory at ../Env/Versions/.
#'
#' @export
#'
#' @examples
#' saveVersion("df")


saveVersion <- function (x)
{
  dir.create(file.path("Env"), showWarnings = FALSE)
  dir.create(file.path("Env", "Versions"), showWarnings = FALSE)
  saveRDS(get(x), paste(file.path("Env", "Versions", paste0(x,
                                                            "_", gsub("[: -]", "", Sys.time(), perl = TRUE), ".rds"))))
}
# To save a timestamped backup file of any R object
# USE: eg. saveVersion("df")


############################################################################

#' latest
#'
#' Restore the latest saved version of an object by name (in quotes) from the subdirectory of your working directory at ../Env/Versions/.
#'
#' @param x The name of an R object in quotes
#'
#' @return The R object is created as a variable in the Global Environment, or replaces the object with the same name.
#' 
#' @export
#'
#' @examples
#' latest("df")


latest <- function (x, Versions_dir = file.path(getwd(),"Env","Versions"))
{
  # Versions_dir = file.path(getwd(),"Env","Versions","save_all_vars") # for restore_all_vars()
  x <- paste0(file.path(Versions_dir, paste0(x, ".rds")))
  latest_fileName <- basename(x)
  latest_ext = tools::file_ext(latest_fileName)
  latest_fileName <- unlist(str_split(latest_fileName, paste0(".",latest_ext)  )[1])[1]


  # one-col df with list of files in the Env/Versions directory:
  list = list.files(Versions_dir,
                    pattern = NULL,
                    all.files = FALSE,
                    full.names = FALSE,
                    recursive = FALSE,
                    ignore.case = FALSE,
                    include.dirs = FALSE,
                    no.. = FALSE) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    setNames("fileNames")
  
  list$extension = tools::file_ext(list$fileNames) # get file extensions in a new column
  
  for (i in seq_along(list$fileNames)){
    list$fileNames[i] = list$fileNames[i]  %>% str_remove(paste0("\\.",list$extension[i]))
  }
  
  list = list %>% tidyr::separate(fileNames, c("fileName","timestamp"), sep = "_(?=[0-9]{14}$)")
  
  list = list %>%
    filter(fileName == latest_fileName) %>%
    top_n(n = 1, wt = timestamp) %>%
    mutate(latest_version = paste0(fileName,"_", timestamp,".",extension))
  
  latest_file = list %>%
    .[1,4]
  
  y <- readRDS(file.path(Versions_dir, latest_file))
  assign(list$fileName[1], y, envir = .GlobalEnv)
}
# To restore the latest version of any R object backed up using saveVersion()
# USE: eg. latest("df")


############################################################################

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

############################################################################

#' restore_all_vars
#'
#' Restores the latest timestamped version of any R object in the save_all_vars subdirectory. To avoid restoring objects you no longer need, manually delete them from the subdirectory before using this function.
#'
#'
#' @return Backups saved in your working directory at ../Env/Versions/ are restored as objects to your Global Environment. Any objects in the Global Environment with the same variable name will be replaced/overwritten so backups should be made if needed.
#'
#' @export
#'
#' @examples
#' restore_all_vars()
#'

restore_all_vars <- function(  Versions_dir = file.path(getwd(),"Env","Versions","save_all_vars") ) {
  latest("vars", Versions_dir = file.path(getwd(),"Env","Versions","save_all_vars"))
  
vars =  vars[-which(str_detect(vars,"^vars$|^save_all_vars$|^restore_all_vars$"))]
  
  for (i in seq_along(vars)) {
    latest(vars[i], Versions_dir = file.path(getwd(),"Env","Versions","save_all_vars"))}
  }
# Restore all R objects backed up using save_all_vars() to the Global Environment
# Use: restore_all_vars()



