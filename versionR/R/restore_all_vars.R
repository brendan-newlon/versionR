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



