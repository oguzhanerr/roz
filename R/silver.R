# Ensure required packages are available
if (!requireNamespace("uuid", quietly = TRUE)) {
  install.packages("uuid")
}
if (!requireNamespace("leaflet", quietly = TRUE)) {
  install.packages("leaflet")
}
if (!requireNamespace("sf", quietly = TRUE)) {
  install.packages("sf")
}
if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
  install.packages("rnaturalearth")
}
if (!requireNamespace("magrittr", quietly = TRUE)) {
  install.packages("magrittr")
}

library(uuid)
library(leaflet)
library(sf)
library(rnaturalearth)
library(magrittr)

#' Add UUID column to make the records unique
#'
#' @param df Data frame. The data frame to modify.
#'
#' @return A data frame with an added UUID column.
#' @export
#'
#' @examples
#' df <- data.frame(poi_name = c("A", "B"))
#' df <- add_uuid(df)
add_uuid <- function(df) {
  df$uuid <- uuid::UUIDgenerate(n = nrow(df))
  return(df)
}

#' Make has_electricity TRUE if it was NULL and if is_connected is TRUE
#'
#' @param df Data frame. The data frame to modify.
#'
#' @return A data frame with updated has_electricity column.
#' @export
#'
#' @examples
#' df <- data.frame(is_connected = c(TRUE, FALSE), has_electricity = c(NA, FALSE))
#' df <- update_electricity(df)
update_electricity <- function(df) {
  df$has_electricity[is.na(df$has_electricity) & df$is_connected == TRUE] <- TRUE
  return(df)
}

#' Check has_electricity FALSE and is_connected TRUE cases
#'
#' @param df Data frame. The data frame to check.
#'
#' @return Data frame with cases where has_electricity is FALSE and is_connected is TRUE.
#' @export
#'
#' @examples
#' df <- data.frame(is_connected = c(TRUE, TRUE), has_electricity = c(FALSE, TRUE))
#' check_electricity_connectivity(df)
check_electricity_connectivity <- function(df) {
  problem_cases <- df[df$has_electricity == FALSE & df$is_connected == TRUE, ]
  return(problem_cases)
}

#' Check if POIs are within the country boundary
#'
#' @param df Data frame. The data frame with POIs.
#' @param country_boundaries sf object. The country boundaries.
#'
#' @return Data frame with POIs outside the country boundaries.
#' @import rnaturalearth
#' @export
#' @examples
#' df <- data.frame(latitude = c(34.05, -15.78),
#'                  longitude = c(-118.24, -47.93))
#' country_boundaries <- sf::st_as_sf(
#'                           rnaturalearth::ne_countries(
#'                               scale = "medium",
#'                               returnclass = "sf"))
#' check_pois_within_boundaries(df, country_boundaries)
check_pois_within_boundaries <- function(df, country_boundaries) {
  poi_sf <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
  within_country <- sf::st_within(poi_sf, country_boundaries)
  outside_pois <- df[lengths(within_country) == 0, ]

  if (nrow(outside_pois) == 0) {
    message("All points of interest are within the country!")
  } else {
    return(outside_pois)
  }
}

#' Create a leaflet map showing all POIs with layers for within and outside the country
#'
#' @param df Data frame. The data frame with POIs.
#' @param country_boundaries sf object. The country boundaries.
#'
#' @return Leaflet map object.
#' @import leaflet
#' @import magrittr
#' @export
create_poi_map <- function(df, country_boundaries) {
  poi_sf <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
  within_country <- sf::st_within(poi_sf, country_boundaries)

  df$within_country <- lengths(within_country) > 0

  map <- leaflet() %>%
    addTiles() %>%
    addCircleMarkers(data = df[df$within_country == TRUE, ], lat = ~latitude, lng = ~longitude, color = "green", group = "Within Country") %>%
    addCircleMarkers(data = df[df$within_country == FALSE, ], lat = ~latitude, lng = ~longitude, color = "red", group = "Outside Country") %>%
    addLayersControl(
      overlayGroups = c("Within Country", "Outside Country"),
      options = layersControlOptions(collapsed = FALSE)
    )
  return(map)
}
