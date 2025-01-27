% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bronze.R
\name{modify_data_types}
\alias{modify_data_types}
\title{Function to modify the data types of columns to match expected types}
\usage{
modify_data_types(df)
}
\arguments{
\item{df}{Data frame. The data frame to modify.}
}
\value{
A data frame with columns modified to match the expected data types.
}
\description{
Function to modify the data types of columns to match expected types
}
\examples{
df <- data.frame(name = c("Alice", "Bob"), lat = c(34.05, 36.77),
                 lon = c(-118.24, -119.41), connectivity = c(1, 0),
                 electricity = c(1, 0), poi_subtype = c("school", "hospital"))
df <- check_approximate_column_names(df)
df_modified <- modify_data_types(df)
}
