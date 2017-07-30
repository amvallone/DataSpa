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
#'
#' @example
#'
#' x<- sample( LETTERS[1:4], 1000, replace=TRUE, prob=c(0.1, 0.2, 0.65, 0.05) )
#' x<-as.factor(x)
#' x<-as.numeric.factor(x)
#' class(x)


as.numeric.factor <- function(x) {as.numeric(as.character(x))}
