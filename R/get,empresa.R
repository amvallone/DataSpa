#' @name get.empresas
#' @rdname get.empresas
#'
#' @title Genera URLs de empresas
#'
#' @description Crea las URLs de las empresas para extraer informacion
#'
#' @param http URL de pagina conteniendo la lista de URLs
#'
#' @return un vector de caracteres conteniendo las URL
#'
#'

get.empresas<-function(http){
			h<-read_html(http)
			l.e<-html_node(h,css="table")
			g<-html_children(html_nodes(html_node(l.e,css="tbody"),css="td"))
			e<-html_attr(g,"href")
			e<-e[complete.cases(e)==TRUE]
			www<-rep("http:",length(e))
			w.e<-paste(www,e,sep="")
			w.e
		}