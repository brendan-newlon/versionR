#' versionR
#'
#' Functions for saving and restoring timestamped versions of R objects within a subfolder of the working directory.
#'
#'
#'
#' @section Save R objects by variable name.
#'
#' @title
#' saveVersion() and latest()
#'
#' @describeIn The two functions saveVersion() and latest() are used to save an object by name (in quotes) and restore the latest version of it. Subdirectories are created within your working directory > Env > Versions to store the R object as a .rds file.
#'
#' @examples
#' saveVersion("df")
#' latest("df")
#'
#' @param "x" The name of an R object in quotes
#' @return The R object is created as a variable in the Global Environment, or replaces the object with the same name.
saveVersion <- function (x)
{
  dir.create(file.path("Env"), showWarnings = FALSE)
  dir.create(file.path("Env", "Versions"), showWarnings = FALSE)
  saveRDS(get(x), paste(file.path("Env", "Versions", paste0(x,
                                                            "_", gsub("[: -]", "", Sys.time(), perl = TRUE), ".rds"))))
}
# To save a timestamped backup file of any R object
# USE: eg. saveVersion("df")

#_________________________________________
latest <- function (x)
{
  x <- paste0(file.path("Env", "Versions", paste0(x, ".rds")))
  latest_fileName <- basename(x)
  latest_fileName <- unlist(str_split(latest_fileName, "\\.")[1])[1]
  latest_version <- setNames(as.data.frame(list.files(file.path("Env",
                                                                "Versions"), pattern = NULL, all.files = FALSE, full.names = FALSE,
                                                      recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE,
                                                      no.. = FALSE)), c("fileNames")) %>% separate(fileNames,
                                                                                                   c("fileName", "timestamp", "extension"), sep = "_|\\.") %>%
    filter(fileName == latest_fileName) %>% top_n(n = 1,
                                                  wt = timestamp) %>% mutate(latest_version = paste(fileName,
                                                                                                    timestamp, sep = "_")) %>% mutate(latest_version = paste(latest_version,
                                                                                                                                                             extension, sep = "."))
  latest_file <- latest_version[1, 4]
  y <- readRDS(file.path("Env", "Versions", latest_file))
  assign(latest_version$fileName, y, envir = .GlobalEnv)
}
# To restore the latest version of any R object backed up using saveVersion()
# USE: eg. latest("df")


#' @section Saving all objects in the Global Environment
#'
#' @title
#' save_all_vars()
#'
#' @describeIn save_all_vars() creates backups of all variables currently in the Global Environment to a subdirectory > Env > Versions > save_all_vars.
#' @examples
#' save_all_vars()

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

#' @title
#' restore_all_vars()
#'
#' @describeIn restore_all_vars() restores the latest timestamped version of any R object in the save_all_vars subdirectory. To avoid restoring objects you no longer need, manually delete them from the subdirectory before using this function.
#' @examples
#' restore_all_vars()

restore_all_vars <- function() {
  x <- "vars"
  x <-
    paste0(file.path("Env", "Versions", "save_all_vars", paste0(x, ".rds")))
  latest_fileName <- basename(x)
  latest_fileName <- unlist(str_split(latest_fileName, "\\.")[1])[1]
  latest_version <-
    setNames(as.data.frame(
      list.files(
        file.path("Env",
                  "Versions", "save_all_vars"),
        pattern = NULL,
        all.files = FALSE,
        full.names = FALSE,
        recursive = FALSE,
        ignore.case = FALSE,
        include.dirs = FALSE,
        no.. = FALSE
      )
    ), c("fileNames")) %>% separate(fileNames,
                                    c("fileName", "timestamp", "extension"), sep = "_|\\.") %>%
    filter(fileName == latest_fileName) %>% top_n(n = 1,
                                                  wt = timestamp) %>% mutate(latest_version = paste(fileName,
                                                                                                    timestamp, sep = "_")) %>% mutate(latest_version = paste(latest_version,
                                                                                                                                                             extension, sep = "."))
  latest_file <- latest_version[1, 4]
  y <-
    readRDS(file.path("Env", "Versions", "save_all_vars", latest_file))
  assign(latest_version$fileName, y, envir = .GlobalEnv)

  for (i in 1:length(vars)) {
    x <- vars[i]
    x <-
      paste0(file.path("Env", "Versions", "save_all_vars", paste0(x, ".rds")))
    latest_fileName <- basename(x)
    latest_fileName <-
      unlist(str_split(latest_fileName, "\\.")[1])[1]
    if (latest_fileName == "vars|save_all_vars|NA|restore_all_vars")
    {
      next
    }
    latest_version <-
      list.files(
        file.path("Env", "Versions", "save_all_vars"),
        pattern = NULL,
        all.files = FALSE,
        full.names = FALSE,
        recursive = FALSE,
        ignore.case = FALSE,
        include.dirs = FALSE,
        no.. = FALSE
      )   %>%
      as.data.frame() %>%
      setNames(c("fileNames"))   %>%     separate(fileNames,
                                                  c("fileName", "extension"), sep = "\\.") %>%
      mutate (timestamp = substr(fileName, nchar(fileName) - 14 + 1, nchar(fileName)))    %>%

      mutate (fileName = substr(fileName, 1, nchar(fileName) - 15))    %>%
      filter(fileName == latest_fileName)  %>%
      top_n(n = 1,
            wt = timestamp)   %>% mutate(latest_version = paste(fileName, timestamp, sep = "_"))   %>% mutate(latest_version = paste(latest_version,
                                                                                                                                     extension, sep = "."))
    latest_file <- latest_version[1, 4]
    y <-
      readRDS(file.path("Env", "Versions", "save_all_vars", latest_file))
    assign(latest_version$fileName, y, envir = .GlobalEnv)
  }
}
# Restore all R objects backed up using save_all_vars() to the Global Environment
# Use: restore_all_vars()



