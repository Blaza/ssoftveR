# Here we will define predictors we use for differentiating shapes

#' We'll define a vector of names of predictor functions which we use.
#' @export
shape_predictors <- c("centroid_distance_variance",
                      "vertex_count",
                      "side_length_variance",
                      "angle_variance",
                      "adj_angle_sum_variance")

#' Get predictors for a shape
#'
#' @param shape - the shape for which to get predictors
#' @param df - whether to return the predictors as data.frame, useful for
#'        predict. If FALSE, returns a named list
#' @param na_replacement - the value to use to replace NAs
#' @return A named vector or dataframe containing all predictors for the shape.
#'         The list of predictors is contained in ssoftveR::shape_predictors.
#' @export
get_shape_predictors <- function(shape, df = FALSE, na_replacement = NA) {
  pred <- sapply(shape_predictors, function(pr) do.call(pr, list(shape)),
                 simplify = FALSE)
  pred[is.na(pred)] <- na_replacement
  if(df) data.frame(pred) else pred
}

#' Variance of the distance from the centroid
#'
#' The idea is that a circle will have almost zero variance of the distance
#' from the center, while other shapes won't.
#'
#' @param shape - the shape for which to calculate the predictor
#' @return The variance of the distances from the centroid of all points on
#'         the contour of the shape
#' @export
centroid_distance_variance <- function(shape) {
  # Assuming we have only one contour for the shape
  contour <- shape$contours[[1]]
  # Calculating distances from the centroid of all points on the contour
  distances <- sqrt((contour$x - shape$centroid['x'])^2 +
                    (contour$y - shape$centroid['y'])^2)
  var(distances) # returning the variance of distances
}


#' Number of possible vertices
#'
#' @param shape - the shape for which to calculate the predictor
#' @return The number of possible vertices found (using the get_vertices()
#'         function). It is returned as a string, because we want it to be
#'         a factor and not a numerical variable.
#' @export
vertex_count <- function(shape) {
  as.character(length(shape$vertices$x))
}


#' Variance of the lengths of sides (distance between vertices)
#'
#' The idea is that regular polygons will have all sides same, i.e. var = 0
#'
#' @param shape - the shape for which to calculate the predictor
#' @return The variance of the distances between adjacent vertices
#' @export
side_length_variance <- function(shape) {
  vertices <- shape$vertices

  if(length(vertices$x) == 0) return(0)

  vertices_adj <- lapply(vertices, shift, n = 1)

  distances <- sqrt((vertices$x - vertices_adj$x)^2 +
                    (vertices$y - vertices_adj$y)^2)

  var(distances) # returning the variance of distances
}


#' Variance of the angles at vertices
#'
#' The idea is that e.g. rectangles have same angles
#'
#' @param shape - the shape for which to calculate the predictor
#' @return The variance of the angles at vertices
#' @export
angle_variance <- function(shape) {
  vertices <- shape$vertices

  if(is.null(vertices$angle)) 0 else var(vertices$angle, na.rm = TRUE)
}


#' Variance of the sum of adjacent angles
#'
#' In paralelograms, the sum of all adjacent angle pairs is 180deg
#'
#' @param shape - the shape for which to calculate the predictor
#' @return The variance of the sum of adjacent angles
#' @export
adj_angle_sum_variance <- function(shape) {
  vertices <- shape$vertices

  if(is.null(vertices$angle)) return(0)

  adj_ang_sum <- vertices$angle + shift(vertices$angle)

  var(adj_ang_sum, na.rm = TRUE)
}

