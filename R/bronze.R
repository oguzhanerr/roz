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

#' Function to check and rename approximate column names to match expected column names
#'
#' @param df Data frame. The data frame with columns to check and rename.
#'
#' @return A data frame with columns renamed to match the expected column names.
#' @export
#'
#' @examples
#' df <- data.frame(name = c("Alice", "Bob"), lat = c(34.05, 36.77),
#'                  lon = c(-118.24, -119.41), connectivity = c(TRUE, FALSE),
#'                  electricity = c(TRUE, TRUE), poi_subtype = c("school", "hospital"))
#' df_renamed <- check_approximate_column_names(df)
check_approximate_column_names <- function(df) {
  # Define mapping of approximate column names to expected column names
  column_mapping <- list(
    poi_name = c("name", "poi_name"),
    latitude = c("lat", "latitude"),
    longitude = c("lon", "longitude"),
    is_connected = c("connectivity", "is_connected"),
    has_electricity = c("electricity", "has_electricity")
  )

  # Rename columns in the data frame
  new_colnames <- sapply(names(df), function(col) {
    match <- FALSE
    for (expected_col in names(column_mapping)) {
      if (col %in% column_mapping[[expected_col]]) {
        match <- TRUE
        return(expected_col)
      }
    }
    if (!match) {
      return(col)
    }
  })
  names(df) <- new_colnames

  return(df)
}

#' Function to modify the data types of columns to match expected types
#'
#' @param df Data frame. The data frame to modify.
#'
#' @return A data frame with columns modified to match the expected data types.
#' @export
#'
#' @examples
#' df <- data.frame(name = c("Alice", "Bob"), lat = c(34.05, 36.77),
#'                  lon = c(-118.24, -119.41), connectivity = c(1, 0),
#'                  electricity = c(1, 0), poi_subtype = c("school", "hospital"))
#' df <- check_approximate_column_names(df)
#' df_modified <- modify_data_types(df)
modify_data_types <- function(df) {
  for (col in names(expected_types)) {
    expected_type <- expected_types[[col]]
    if (is.list(expected_type)) {
      expected_type <- expected_type$type
    }

    if (expected_type == "character") {
      df[[col]] <- as.character(df[[col]])
    } else if (expected_type == "numeric") {
      df[[col]] <- as.numeric(df[[col]])
    } else if (expected_type == "logical") {
      df[[col]] <- as.logical(df[[col]])
    }
  }
  return(df)
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


#' Function to transform all character data in a data frame to lower case
#'
#' @param df Data frame. The data frame to transform.
#'
#' @return A data frame with all character data transformed to lower case.
#' @export
#'
#' @examples
#' df <- data.frame(Name = c("Alice", "Bob"), City = c("New York", "Los Angeles"))
#' df_lower <- transform_to_lower_case(df)
transform_to_lower_case <- function(df) {
  df[] <- lapply(df, function(x) {
    if (is.character(x)) {
      tolower(x)
    } else {
      x
    }
  })
  return(df)
}

#' Function to export the final data frame to a CSV file
#'
#' @param df Data frame. The data frame to export.
#' @param file_path Character. The path to the file where the data frame will be saved.
#'
#' @export
export_final_df <- function(df, file_path) {
  readr::write_csv(df, file_path)
  message("Data frame exported successfully to ", file_path)
}
