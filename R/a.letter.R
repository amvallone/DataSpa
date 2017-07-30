#' @name a.letter
#' @rdname a.letter
#'
#' @title Quita las letras ñ y los espacios
#'
#' @param x un vector de caracteres
#'
#' @description busca y quita las ñ por n y remplaza los espacion por -
#' @return un vector de caracteres

a.letter<-function(x){
		if(str_detect(x," ")==TRUE){
		x<-str_replace_all(x," ","_")
		}
	    if(str_detect(x,"\u00D1")==TRUE){
		x<-str_replace_all(x,"\u00D1","N")
		}
		x
}
