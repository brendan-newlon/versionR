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
