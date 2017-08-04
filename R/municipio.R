#' @name muncipio
#' @rdname municipio
#'
#' @title List of muncipalities URLs in Axesor firms data base
#'
#' @description Generate a list muncipalities URLs in Axesor firms data base
#'
#' @param http character containing a province URL in Axesor firms data nase
#'
#' @return no me acuerdo
#'
#' @examples
#' municipio("http://www.axesor.es/directorio-informacion-empresas/empresas-de-Alava")
#'
#' @export

municipio<-function(http){
		base.m<-html_nodes(read_html(http),css="#bloque_listadoMunicipios")
		l.m<-html_attr(html_children(html_nodes(base.m,css="td")),"href")
		mun<-rep(NA,length(l.m))
		for (j in 1:length(l.m)){
			mun[j]<-paste("http:",l.m[j],sep="")
		}
		return(mun)
	}