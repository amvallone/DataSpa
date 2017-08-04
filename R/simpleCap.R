#' @name simpleCap
#' @rdname simpleCap
#'
#' @title Coloca la primer letra en mayuscula
#'
#' @description coloca la primer letra en mayuscula de un vector de caracteres
#'
#' @param x un vector de caracteres
#'
#' @return un vector de caracteres
#'
#'

simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(substring(s, 1, 1), tolower(substring(s, 2)),
          sep = "", collapse = " ")
}