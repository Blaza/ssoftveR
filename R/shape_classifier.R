#' @export
shape_code_map <- list("et" = "equilateral triangle",
                       "rt" = "right triangle",
                       "tr" = "triangle",
                       "sq" = "square",
                       "re" = "rectangle",
                       "rh" = "rhombus",
                       "pa" = "paralelogram",
                       "tp" = "trapezium",
                       "ci" = "circle",
                       "el" = "ellipse",
                       "pe" = "pentagon",
                       "he" = "hexagon")

#' Classify a shape
#'
#' Classifies the shape into one of the categories which can be seen from
#' shape_code_map, or Unknown if it has more than one contour
#'
#' @param shape - the shape to classify
#' @return A character vector with the code and long name of the shape class
#' @export
classify_shape <- function(shape) {
  # if the shape has more than one contour, we don't consider it a shape
  if(length(shape$contours) != 1) return(c(code = NA, long_name = "Unknown"))

  # In the model we used -1 for non existant values, e.g. angle of circle
  shape_pred <- get_shape_predictors(shape, df = TRUE, na_replacement = -1)

  shape_class <- as.character(predict(shapes_model, shape_pred))

  c(code = shape_class, long_name = shape_code_map[[shape_class]])
}

