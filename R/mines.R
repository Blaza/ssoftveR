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
extract_fields <- function(im, boundaries = get_boundaries(im)) {
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
#' Clip image around the border given the amount
#'
#' 'amount' can be a single value or a vector of size 4. If only one value is
#' given, all sides are clipped with the same amount. If it is a vector with
#' 4 values, the sides are clipped by amount, respectively, in the order
#' left, right, top, bottom.
#'
#' @param im - the image to clip
#' @param amount - the number of pixels to clip, as numeric, or, if character,
#'                 the percentage of dimension of the image (e.g.
#'                 clip(im, "5%")).
#' @return The clipped image
#' @export
clip <- function(im, amount = 0) {
  if (!length(amount) %in% c(1, 4))
    stop("amount must be a single value or a 4-element vector")

  w <- dim(im)[1]
  h <- dim(im)[2]

  n <- amount
  q <- rep(1, length(amount)) # quotient
  if(is.character(amount)) {
    percent <- as.numeric(gsub("^(\\d+(\\.\\d*)?|\\.\\d+)%$","\\1", amount))
    q <- percent / 100
  }

  if (length(n) == 1) {
    n <- round(min(w , h) * q)
    as.cimg(im[n : (w - n), n : (h - n), , , drop = FALSE])
  } else {
    n <- round(c(w * q[1], w * q[2], h * q[3], h * q[4]))
    as.cimg(im[n[1] : (w - n[2]), n[3] : (h - n[4]), , , drop = FALSE])
  }
}


#' Internal function used to slide a matrix in a single direction.
#'
#' If you want to slide a matrix, use slide_matrix
slide_matrix_single <- function(mat, dir, fill) {
  h <- dim(mat)[1] # height of mat
  w <- dim(mat)[2] # width of mat
  dir <- unlist(dir) # ensure dir is a vector (adply gives data.frame)
  row <- dir['row'] # row component of translation
  col <- dir['col'] # col component of translation

  # prepopulate the resulting matrix with fill values
  sm <- matrix(rep(fill, h*w), ncol = w)

  # calculate ranges which are to be populated in the slid matrix
  row_range <- if (row < 0) 1 : (h + row) else (row + 1) : h
  col_range <- if (col < 0) 1 : (w + col) else (col + 1) : w

  # populate given ranges in sm with appropriate values from original matrix
  sm[row_range, col_range] <- mat[row_range - row, col_range - col]

  sm
}


#' Slide a matrix in a given direction
#'
#' Translates a matrix in a given direction (or directions)
#'
#' If simplify = TRUE, than this returns a matrix where each column is a matrix
#' unrolled into a vector. If simplify = FALSE, this returns a list of matrices
#' resulted from sliding the given matrix in given directions.
#'
#' Directions should be a dataframe with columns named 'row' and 'col' and each
#' row in the dataframe is a row-col vector which gives the direction for
#' translation. If 'row' and 'col' aren't the names of columns, the first column
#' in the dataframe will be treated as 'row' and the second as 'col'.
#'
#' If only one direction is given, it needn't be a dataframe, but can be just a
#' vector with row-col coordinates.
#'
#' @param mat - the matrix to slide
#' @param directions - a dataframe of the directions in which to slide the
#'                     matrix, each row being the row-column coordinates of a
#'                     direction vector (see details)
#' @param fill - the value with which to fill empty space left from sliding the
#'               given matrix
#' @param simplify - whether to merge results into a matrix or return a list
#'                   of slidden matrices (see details)
#' @return Matrix (or list of matrices) which represent mat translated in given
#'         direction(s). See details.
#' @export
slide_matrix <- function(mat, directions, fill = NA, simplify = FALSE) {
  if (is.vector(directions)) {
    if (!all(c('row', 'col') %in% names(directions)))
      names(directions) <- c("row", "col")
    directions <- data.frame(row = unname(directions['row']),
                             col = unname(directions['col']))
  } else {
    if (!all(c('row', 'col') %in% colnames(directions)))
      colnames(directions) <- c("row", "col")
  }


  # turn directions into a list. apply simplifies stuff, so this step is
  # necessary to return a list of slid_matrices
  slid_matrices <- plyr::alply(directions, 1, slide_matrix_single,
                               mat = mat, fill = fill)
  names(slid_matrices) <- plyr::aaply(directions, 1, function(rc) {
                                    rc <- unlist(rc)
                                    paste0("(", rc["row"], ",", rc["col"], ")")
                          })

  if (simplify) {
    slid_matrices <- sapply(slid_matrices, as.vector)
  } else if (length(slid_matrices) == 1) {
    slid_matrices <- slid_matrices[[1]]
  }

  slid_matrices
}

