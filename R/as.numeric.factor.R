#' name as.numeric.factor
#' rdname extras
#'
#' @title Convert a factor vector into a numeric vector
#'
#' @description trasnform a vector of factor into a numerical vector
#'
#' @param x a factor vector
#'
#' @return a numerical vector



as.numeric.factor <- function(x) {as.numeric(as.character(x))}
