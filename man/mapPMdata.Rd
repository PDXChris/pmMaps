% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/baseMap2.r
\name{mapPMdata}
\alias{mapPMdata}
\title{Map PAWMAP data}
\usage{
mapPMdata(dfm, vbl, loadObj = NULL, GISdir = NULL, ldRivers = TRUE,
  ldStreams = FALSE)
}
\arguments{
\item{dfm}{Data frame with data to be mapped}

\item{vbl}{The field containing the value to map}

\item{loadObj}{Path of any data objects that need to be loaded (e.g., wq14)}

\item{GISdir}{Path to GIS data}

\item{ldRivers}{Should the river shapefile be loaded?}

\item{ldStreams}{Should the streams shapefile be loaded?}
}
\description{
Map PAWMAP data
}
