.onLoad <- function(libname, pkgname) {
  # Define expected columns
  expected_columns <<- c("poi_name", "poi_subtype", "latitude", "longitude", "is_connected", "has_electricity")

  # Define expected types for each column
  expected_types <<- list(
    poi_name = "character",
    poi_subtype = list(type = "character", values = c("school", "hospital", "post office")),
    latitude = "numeric",
    longitude = "numeric",
    is_connected = "logical",
    has_electricity = "logical"
  )
}
