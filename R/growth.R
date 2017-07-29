#' @name growth
#' @rdname growth
#' 
#' @title Growth rate of an variable
#'
#' @description Compute the groth rate of n periods od an x variable
#'
#' @param x a numeric vector corrspondin to \eqn{t}{t} time of a variable
#' @param y a numeric vector corrsponding to \eqn{t+n}{t+n} time in a varible
#' @param n period of time between x and y
#'
#' @return a growth rate, basically is the compute of an compound interest rate 

growth<-function(x,y,n){
	z<-(((x/y)^(1/n))-1)*100
	z
	}