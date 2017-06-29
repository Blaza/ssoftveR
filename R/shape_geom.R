#' Get sides of shape
#'
#' Sides (straight lines in the shape) are sets of points where the slope
#' doesn't change significantly
#'
#' @param shape - the shape for which to find vertices
#' @param ma - number of moving average points used for smoothing. Must be odd.
#' @param vel_n - number of points before and after current to use for
#'                approximating velocity vector
#' @return A list of objects of class 'side', each containing the x and y
#'         coordinates of the points of the side of the shape, and also the
#'         approximate slope angle of the line.
#' @export
get_sides <- function(shape, ma = 7, vel_n = 2) {
  # Assuming one contour (also closed)
  contour <- shape$contours[[1]]
  # We get only points which are at least 1 pixel apart
  distances <- sqrt(diff(contour$x)^2 + diff(contour$y)^2)
  contour$x <- contour$x[which(distances >= 1)]
  contour$y <- contour$y[which(distances >= 1)]

  cnt_len <- length(contour$x)

  # we smooth the contour, as it is on a discrete net
  contour_ma <- list()
  contour_ma$x <- moving_average(contour$x, n = ma)
  contour_ma$y <- moving_average(contour$y, n = ma)

  range <- setdiff(-vel_n:vel_n, 0)
  vel_x <- rowMeans(sapply(range, function(r) {
                              sign(r) * (shift(contour_ma$x, r) - contour_ma$x)
                     }))
  vel_y <- rowMeans(sapply(range, function(r) {
                              sign(r) * (shift(contour_ma$y, r) - contour_ma$y)
                     }))

  vel <- cbind(vel_x, vel_y)
  unit_vel <- vel / sqrt(rowSums(vel^2))

  # We'll say that a difference in velocity vectors isn't significant if the
  # euclidean distance between them is less than 0.1
  max_dist <- 0.1
  # so we get points where the lines may break
  break_points <- sqrt(rowSums(diff(unit_vel)^2)) > max_dist

  # we get run lengths of TRUE/FALSE values
  rl <- rle(break_points)

  # we merge run lengths if the first and last runs are same, i.e. we have a
  # cyclic run (because the contour is closed)
  old_lengths <- rl$lengths
  if(tail(rl$values, 1) == rl$values[1]) {
    rl$lengths[1] = rl$lengths[1] + tail(rl$lengths, 1)
    # remove the last element as it is merged
    rl$values <- head(rl$values, -1)
    rl$lengths <- head(rl$lengths, -1)
    old_lengths <- head(old_lengths, -1)
  }

  # get end indices of all runs
  ind <- cumsum(old_lengths)

  # we get the middle index in the run of TRUE values as the break (a vertex)
  break_indices <- (ind[rl$values] - rl$lengths[rl$values]%/%2) %% cnt_len

  sides <- apply(cbind(break_indices, shift(break_indices, 1)), 1, function(v) {
                   # get indices for elements of sides
                   side_ind <- if(v[2] < v[1]) {
                                 (v[1] : (cnt_len + v[2])) %% cnt_len
                               } else {
                                 v[1] : v[2]
                               }
                   # get indices of middle 75% of elements of sides, as they're
                   # more stable points with regard to velocity vector
                   len <- round(0.75 * length(side_ind))
                   m_ind <- (length(side_ind) - len) %/% 2 + 1 : len
                   mid_ind <- side_ind[m_ind]
                   # build the list containing current side
                   side <- list(x = contour$x[side_ind],
                                y = contour$y[side_ind],
                                # we take the mean of all slopes of velocity
                                # vectors of middle (more stable) side points
                                slope = mean(atan( unit_vel[mid_ind, 2] /
                                                   unit_vel[mid_ind, 1] )) )
                   # we want a slope between 0 and pi
                   #side$slope <- side$slope %% pi
                   class(side) <- c('side', class(side))
                   side
            })
  sides
}


#' Get possible vertices
#'
#' Possible vertices are points where the slope of the contour of the shape
#' changes significantly
#'
#' @param shape - the shape for which to find vertices
#' @param ... - additional parameters passed to get_lines
#' @return A list containing the x and y coordinates of vertices, and the
#'         approximate angles at the respective vertices
#' @export
get_vertices <- function(shape, ...) {
  sides <- get_sides(shape, ...)
  vertices <- list(x = sapply(sides, function(s) s$x[1]),
                   y = sapply(sides, function(s) s$y[1]))
  vertices
}

