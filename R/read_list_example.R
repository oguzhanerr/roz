#' Function that reads or lists the example files
#'
#' @param path Character. The name of the file to read from the extdata directory of the readr package. If NULL, lists all example files.
#'
#' @return If the path is NULL, returns a character vector of the names of example files. If the path is specified, returns the full path to the specified example file.
#' @export
#'
#' @examples
#' read_example()
#'
#' read_example("raw_data.csv")
read_example <- function(path = NULL) {
  if (is.null(path)) {
    return(dir(system.file("extdata", package = "roz")))
  } else {
    file_path <- system.file("extdata", path, package = "roz", mustWork = TRUE)
    if (file_path == "") {
      stop("File not found: ", path)
    }
    return(file_path)
  }
}
