#' Get coordinates of boundaries of fields
#'
#' Determines the x and y ranges of each field
#'
#' @param im - the image from which to determine boundaries (must have exactly
#'             one color channel)
#' @param eps - the highest value to treat as black
#' @param prob - the ratio of number of 'black' pixels in a line to the length
#'               of the line
#' @return An object (list) of class 'bounds' containing 81 elements, each
#'         having a $x and $y attributes giving, respectively, the x and y
#'         range of the boundaries.
#' @export
get_boundaries <- function(im, eps = 0.1, prob = 0.8) {
  if (dim(im)[4] != 1)
    stop("The input image must have only one channel.
         You may want to grayscale it first")

  # get vertical lines x coordinates
  vert <- which( colMeans(im[ , , 1, 1] < eps) > prob )
  # get horizontal lines y coordinates
  hori <- which( rowMeans(im[ , , 1, 1] < eps) > prob )

  vert_diff <- diff(vert)
  hori_diff <- diff(hori)

  # get indices of left edges. We take the index where there was a leap in diff,
  # FALSE is there to fix dimensions, and we add 1 so we skip the black pixel.
  L <- vert[c(vert_diff > 1, FALSE)] + 1
  # get indices of right edges. Similar to the previous line, just that for L
  # we took the left point of the jump, whereas now we take the right point
  # (because FALSE is before vert_diff > 1).
  R <- vert[c(FALSE, vert_diff > 1)] - 1

  # combine left and right edge indices in matrix, each row being L-R vector
  x_bounds <- cbind(L, R)

  # We do an analogous procedure for y edges
  y_bounds <- cbind( U = hori[c(hori_diff > 1, FALSE)] + 1,
                     D = hori[c(FALSE, hori_diff > 1)] - 1 )

  # create a data.frame of 9x9 matrix coordinates
  coords <- expand.grid(row = 1:9, col = 1:9)

  # return the list with coordinates and x and y boundary ranges
  ret <- apply(coords, 1, function(rc) {
                 list(coords = rc,
                      x = x_bounds[rc['col'], ],
                      y = y_bounds[rc['row'], ])
         })
  class(ret) <- c("bounds", class(ret))
  ret
}


#' Extract fields from an image, given the boundaries
#'
#' @param im - the image from which to extract fields, using the boundaries
#' @param boundaries - the object of class 'bounds' (from get_boundaries
#'                     function), containing x and y ranges of field coordinates
#' @return A list containing containing 'field' objects, i.e. lists with
#'         matrix coordinates of the field and the image of the field
#' @export
extract_fields <- function(im, boundaries) {
  if (!"bounds" %in% class(boundaries))
    stop("'boundaries' must be of class 'bounds'.")

  lapply(boundaries, function(bnd) {
           x_range <- bnd$x[1] : bnd$x[2]
           y_range <- bnd$y[1] : bnd$y[2]
           field <- list(coords = bnd$coords,
                         image = as.cimg(im[x_range, y_range, , ,
                                            drop = FALSE]))
           class(field) <- c("field", class(field))

           field
  })
}


#' Calculate specified predictors for given fields
#'
#' Takes a vector of functions which represent predictors and applies them to
#' the given list of fields
#'
#' @param predictors - a character vector containing the names of functions that
#'                     take a field$image and return the value of a predictor
#' @param fields - a list containing objects of class 'field', usually got from
#'                the extract_fields function
#' @param rename_rows - whether to name rows as (i, j) indicating location in
#'                      a 9x9 matrix (only use for length(fields) == 81)
#' @return A matrix with each row being the calculated predictors for a field,
#'         populated column-by-column, just as a regular R matrix, so the
#'         (i,j) field predictors are in the (i + 9*(j-1))-th row of the matrix.
#' @export
get_field_predictors <- function(predictors, fields, rename_rows = TRUE) {
  # we transpose to get the desired structure (each row = predictors)
  ret <- sapply(predictors, function(pred) {
                sapply(fields, function(field) {
                    do.call(pred, list(field$image))
                })
         })
  ret <- as.data.frame(ret)

  if (rename_rows && length(ret[ , 1]) == 81) {
  rownames(ret) <- apply(expand.grid(1:9, 1:9), 1, function(rc)
                         paste0("(", rc[1], ", ", rc[2], ")"))
  }
  ret
}


#' Clip image border
#'
#' @param im - the image to clip
#' @param amount - the number of pixels to clip form each side, as numeric, or,
#'                 if character, the percentage of the smaller dimenision of
#'                 the image (e.g. clip(im, "5%"))
#' @return The clipped image
#' @export
clip <- function(im, amount = 0) {
  w <- dim(im)[1]
  h <- dim(im)[2]

  n <- amount
  if(is.character(amount)) {
    percent <- as.numeric(gsub("^(\\d+(\\.\\d*)?|\\.\\d+)%$","\\1", amount))
    n <- round(min(w, h) * percent / 100)
  }

  as.cimg(im[n : (w - n), n : (h - n), , , drop = FALSE])
}

