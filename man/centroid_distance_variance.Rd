% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shape_predictors.R
\name{centroid_distance_variance}
\alias{centroid_distance_variance}
\title{Variance of the distance from the centroid}
\usage{
centroid_distance_variance(shape)
}
\arguments{
\item{shape}{- the shape for which to calculate the predictor}
}
\value{
The variance of the distances from the centroid of all points on
        the contour of the shape
}
\description{
The idea is that a circle will have almost zero variance of the distance
from the center, while other shapes won't.
}
