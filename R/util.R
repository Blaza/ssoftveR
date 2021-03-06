#' Shift a vector cyclically
#'
#' @param x - the vector to shift
#' @param n - the number of places to shift  (can be negative)
#' @return A vector of the same size as x, shifted cyclically by n places (e.g.
#'         (1,2,3) shifted by n = 1 is (2,3,1))
#' @export
shift <- function(x, n = 0) {
  if (n == 0) x else c(tail(x, -n), head(x, n))
}

#' Smooth vector using moving averages
#'
#' @param x - the vector to smooth
#' @param n - the number of points to use for smoothing. Must be odd.
#' @param cyclic - logical indicating whether to treat the vector as cyclic
#' @return A vector of centered averages of size n. If cyclic=TRUE, it is the
#'         same length as vector x and the last elements are averages of first
#'         and last elements (cyclical overflow).
#' @export
moving_average <- function(x, n = 5, cyclic = TRUE) {
  range <- -(n%/%2) : (n%/%2)
  # We run shift(x, r) for each element r of range and combine the returned
  # vectors into a matrix.
  shift_matrix <- sapply(range, shift, x = x)

  # return means by row
  rowMeans(shift_matrix)
}

#' Get the nearest color name from rgb
#'
#' @param rgb - vector with r,g,b components, in that order
#' @param color_names - a character vector containing the list of color names
#'                      to look from. By default it's all colors R knows about
#' @export
rgb2col <- function(rgb, color_names = grDevices::colors()) {
  # define a matrix with rgb values of colors R knows about.
  # Each column is a RGB color vector
  colors_rgb <- grDevices::col2rgb(color_names)

  # a fancy metric for distance between two colors, should resemble human
  # perception of color. See https://en.wikipedia.org/wiki/Color_difference
  metric <- colorscience::deltaE2000

  distances <- apply(colors_rgb, 2, metric, Lab1 = rgb)

  # we return the color for which the distance is lowest
  color_names[which.min(distances)]
}

