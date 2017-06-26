#' Function for plotting shape objects
#'
#' Plots the pixset component of the object in black, and the contours in red.
#'
#' @param shape - the object of class shape to plot
#' @export
plot.shape <- function(shape) {
  plot(shape$pixset)
  # we don't want a return value, so we use invisible
  invisible(lapply(shape$contours, points, col = 'red', pch = 16, cex = 0.5))
}


#' Find solid color connected areas in the image
#'
#' Turns the image object into a logical image (pixset) of blobs of pixels of
#' the same color
#'
#' Each pixel maps to TRUE if every pixel in the stencil neighbourhood is
#' the same color, otherwise it becomes FALSE.
#'
#' Some erosion will happen because of the square neighbourhood
#'
#' @param img - the cimg imager object
#' @param stencil - a stencil dataframe which defines the neighbourhood we use.
#'                  Must have a dx and dy columns, like for the imager package
#' @param cr_n - if set, the stencil becomes a "n x n" cross (+) neighbourhood,
#'               centered at current pixel. Should be an odd number.
#' @return A pixset where every pixel is TRUE if the neighbourhood is of the
#'         same color as the pixel
#' @export
solid_blobs <- function(img, stencil = NULL,
                        cr_n = max(min(dim(img)[1:2]) %/% 100, 3)) {
  if(is.null(stencil)) {
    range <- -(cr_n%/%2) : (cr_n%/%2)
    len <- length(range)
    stencil <- data.frame(dx = c(rep(0, len), range),
                          dy = c(range, rep(0, len)))
  }

  if(!all(c('dx', 'dy') %in% colnames(stencil))) {
    stop("stencil must have 'dx' and 'dy' columns!")
  }

  # must ensure that first column is dx and second is dy, for C code
  stencil_matrix <- matrix(c(stencil[, 'dx'], stencil[, 'dy']), ncol = 2)

  img_flat <- imager::flatten.alpha(img)
  C_solid_blobs(img_flat, stencil_matrix)
}

#' Make a 'shape' object from a pixset
#'
#' Turns a pixset into a object of class 'shape', extracting xy coordinates of
#' pixels and contours of the pixset
#'
#' @param px - the pixset to turn into a shape
#' @return An object (list) of class 'shape', containing the pixset itself, the
#'         xy coordinates of the pixset and the contours of the pixset.
#' @export
shape_from_pixset <- function(px) {
  shape <- list(pixset = px,
                xy = imager::where(px),
                contours = imager::contours(px))
  class(shape) <- c("shape", class(shape))
  shape
}

#' Get a list of shapes on the image
#'
#' Gets a list of pixsets representing connected areas in the image
#'
#' @param img - the cimg imager object
#' @param min_area - the minimum area (number of pixels) of a connected object
#'                   that can be considered a shape
#' @param ... - additional parameters passed to solid_blobs (stencil/cr_n)
#' @return A list of pixsets, each representing one shape (connected set
#'         of pixels, excluding background)
#' @export
get_shapes <- function(img, min_area = 25, ...) {
  blobs <- solid_blobs(img, ...)
  # create a list of pixsets representing possible shapes
  sl <- imager::split_connected(blobs) %>% purrr::discard(~ sum(.) < min_area)

  # return a list of objects of class 'shape' from pixsets in sl
  lapply(sl, shape_from_pixset)
}



