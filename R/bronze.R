# Ensure the readr package is available
if (!requireNamespace("readr", quietly = TRUE)) {
  stop("The readr package is required but is not installed. Please install it using install.packages('readr').")
}

#' Read or list the CSV files
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
    message("Listing example files...")
    example_files <- dir(system.file("extdata", package = "roz"))
    message("Example files found: ", paste(example_files, collapse = ", "))
    return(example_files)
  } else {
    file_path <- system.file("extdata", path, package = "roz", mustWork = TRUE)
    if (file_path == "") {
      stop("File not found: ", path)
    }
    message("File path resolved: ", file_path)
    return(file_path)
  }
}

#' Function to validate data types of data frame columns
#'
#' @param df Data frame. The data frame to validate.
#'
#' @return TRUE if all columns match the expected data types, otherwise an error message.
#' @export
#'
#' @examples
#' df <- data.frame(poi_name = "A", poi_subtype = "school", latitude = 34.05, longitude = -118.24,
#'                  is_connected = TRUE, has_electricity = TRUE)
#' validate_data_types(df)
validate_data_types <- function(df) {
  all_checks_passed <- TRUE
  for (col in names(expected_types)) {
    expected_info <- expected_types[[col]]
    if (is.list(expected_info)) {
      expected_type <- expected_info$type
      expected_values <- expected_info$values
    } else {
      expected_type <- expected_info
      expected_values <- NULL
    }
    actual_type <- class(df[[col]])[1]
    if (actual_type != expected_type) {
      message("Type mismatch in column '", col, "': expected '", expected_type, "', got '", actual_type, "'.")
      all_checks_passed <- FALSE
    }
    if (!is.null(expected_values) && !all(df[[col]] %in% expected_values)) {
      invalid_values <- unique(df[[col]][!df[[col]] %in% expected_values])
      message("Invalid values in column '", col, "': ", paste(invalid_values, collapse = ", "), ".")
      all_checks_passed <- FALSE
    }
  }
  if (all_checks_passed) {
    message("All data types are correct and valid.")
  } else {
    stop("Data type validation failed.")
  }
  return(TRUE)
}

#' Function to read a CSV file, transform it into a data frame, and validate columns and data types
#'
#' @param file_path Character. The path to the CSV file to be read.
#'
#' @return A data frame with only the expected columns if they match, otherwise an error message.
#' @export
#'
#' @examples
#' file_path <- read_example("raw_data.csv")
#' df <- transform_csv_to_df(file_path)
transform_csv_to_df <- function(file_path) {
  expected_columns <- roz:::expected_columns
  expected_types <- roz:::expected_types

  # Read the CSV file into a data frame
  message("Reading CSV file: ", file_path)
  df <- readr::read_csv(file_path)
  message("CSV file read successfully.")

  # Check if the columns in the data frame match the expected columns
  actual_columns <- colnames(df)
  if (!all(expected_columns %in% actual_columns)) {
    missing_columns <- setdiff(expected_columns, actual_columns)
    stop("The following expected columns are missing: ", paste(missing_columns, collapse = ", "))
  }
  message("All expected columns are present.")

  # Select only the expected columns and drop any unnecessary columns
  df <- df[, expected_columns, drop = FALSE]
  message("Unnecessary columns dropped. Data frame now contains only the expected columns.")

  # Validate data types using the separate function
  validate_data_types(df)

  # Return the data frame
  message("Data frame validation successful.")
  return(df)
}
