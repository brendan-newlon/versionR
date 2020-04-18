#' latest
#'
#' Restore the latest saved version of an object by name (in quotes) from the subdirectory of your working directory at ../Env/Versions/.
#'
#' @param x The name of an R object in quotes
#'
#' @return The R object is created as a variable in the Global Environment, or replaces the object with the same name.
#'
#' @import(magrittr)
#' @import(stringr)
#' @import(tools)
#' @import(tidyr)
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


  # one-col df with list of files in the Env/Versions directory:
    list = list.files(file.path("Env", "Versions"),
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


  y <- readRDS(file.path("Env", "Versions", latest_file))
  assign(list$fileName[1], y, envir = .GlobalEnv)
}
# To restore the latest version of any R object backed up using saveVersion()
# USE: eg. latest("df")

