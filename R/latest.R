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

