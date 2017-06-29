# Here we will define predictors we use for differentiating shapes

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
#'         function)
#' @export
vertex_count <- function(shape) {
  length(get_vertices(shape)$x)
}
