#' Find solid color connected areas in the image
#'
#' Turns the image object into a logical image (pixset) of blobs of pixels of
#' the same color
#'
#' Each pixel maps to TRUE if every pixel in the square 3x3 neighbourhood is
#' the same color, otherwise it becomes FALSE.
#'
#' Some erosion will happen because of the square neighbourhood
#'
#' @param img - the cimg imager object
#' @return A pixset where every pixel is TRUE if the neighbourhood is of the
#'         same color as the pixel
#' @export
solid_blobs <- function(img) {
  img_flat <- imager::flatten.alpha(img)
  C_solid_blobs(img_flat)
}

#' Get a list of shapes on the image
#'
#' Gets a list of pixsets representing connected areas in the image, aka shapes
#'
#' @param img - the cimg imager object
#' @return A list of pixsets, each representing one shape (connected set
#'         of pixels, excluding background)
#' @export
get_shapes <- function(img) {
  blobs <- solid_blobs(img)
  imager::split_connected(blobs)
}


