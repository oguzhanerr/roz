% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/silver.R
\name{create_poi_map}
\alias{create_poi_map}
\title{Create a leaflet map showing all POIs with layers for within and outside the country}
\usage{
create_poi_map(df, country_boundaries)
}
\arguments{
\item{df}{Data frame. The data frame with POIs.}

\item{country_boundaries}{sf object. The country boundaries.}
}
\value{
Leaflet map object.
}
\description{
Create a leaflet map showing all POIs with layers for within and outside the country
}
