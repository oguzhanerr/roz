# Ensure the readr package is available
if (!requireNamespace("readr", quietly = TRUE)) {
  stop("The readr package is required but is not installed. Please install it using install.packages('readr').")
}

#' Function that reads (or lists examples) the csv files and transforms to a dataframe
#'
#' @param path Character. The name of the file to read from the extdata directory of the roz package. If NULL, lists all example files.
#'
#' @return If the path is NULL, returns a character vector of the names of example files. If the path is specified, returns the full path to the specified example file.
#' @import readr
#' @export
#'
#' @examples
#' read_example()
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

#' Function to read a CSV file and transform it into a data frame
#'
#' @param file_path Character. The path to the CSV file to be read.
#' @param expected_columns Character vector. The expected column names in the data frame.
#'
#' @return A data frame if the columns match the expected ones, otherwise an error message.
#' @export
#'
#' @examples
#' file_path <- read_example("raw_data.csv")
#' expected_columns <- c("poi_name", "poi_subtype", "latitude", "longitude", "is_connected", "has_electricity")
#' transform_csv_to_df(file_path, expected_columns)
transform_csv_to_df <- function(file_path, expected_columns) {
  # Read the CSV file into a data frame
  df <- readr::read_csv(file_path)

  # Check if the columns in the data frame match the expected columns
  actual_columns <- colnames(df)
  if (!all(expected_columns %in% actual_columns)) {
    missing_columns <- setdiff(expected_columns, actual_columns)
    stop("The following expected columns are missing: ", paste(missing_columns, collapse = ", "))
  }

  # Select only the expected columns and drop any unnecessary columns
  df <- df[, expected_columns, drop = FALSE]

  # Return the data frame
  return(df)
}
