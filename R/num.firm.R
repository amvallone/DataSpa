#' @name num.firm
#' @rdname num.firm
#'
#' @title Collects the set of firms at a municipality level
#'
#' @description \code{num.firm} generates a data frame with the set of firms in a selected municipality
#'
#' @param provincia one of the 52 Spanish provinces. See \link{getbase.pob} for details.
#'
#'
#' @return It is a data frame 
#'
#' @family firm functions
#' @examples
#' \dontrun{num.firm("Araba")}
#'
#' @export



num.firm <- function(provincia){
	prov <- toupper(provincia)
	b <- "http://www.axesor.es/directorio-informacion-empresas/empresas-de-"
	p <- c("albacete","alicante","almeria","alava","asturias","avila","badajoz","baleares","barcelona", "vizcaya","burgos","caceres","cadiz","cantabria","castellon","ciudad-real","cordoba","la-coruna","cuenca","guipuzcoa","girona","granada","guadalajara","huelva","huesca","jaen","leon","lleida","lugo","madrid","malaga","murcia","navarra","orense","palencia","las-palmas","pontevedra","la-rioja","salamanca","santa-cruz-de-tenerife","segovia","sevilla","soria","tarragona","teruel","toledo","valencia","valladolid","zamora","zaragoza","ceuta","melilla")
	names(p) <- c("ALBACETE","ALICANTE","ALMERIA","ARABA","ASTURIAS","AVILA","BADAJOZ","BALEARES","BARCELONA","BIZKAIA","BURGOS","CACERES","CADIZ","CANTABRIA","CASTELLO","CIUDAD REAL","CORDOBA","A CORU\u00D1A","CUENCA","GIPUZKOA","GIRONA","GRANADA","GUADALAJARA","HUELVA","HUESCA","JAEN","LEON","LLEIDA","LUGO","MADRID","MALAGA","MURCIA","NAVARRA","OURENSE","PALENCIA","LAS PALMAS","PONTEVEDRA", "LA RIOJA","SALAMANCA","TENERIFE","SEGOVIA","SEVILLA","SORIA","TARRAGONA","TERUEL","TOLEDO","VALENCIA","VALLADOLID","ZAMORA","ZARAGOZA","CEUTA","MELILLA")
	url <- paste(b,p[prov],sep="")
	mun <- municipio(url)
	aux <- sapply(mun,lista.empresa)
	n <- sapply(aux,length)
	names(n) <- NULL
	names.aux <- html_nodes(read_html(url),css="#bloque_listadoMunicipios")
	names <- html_text(html_children(html_nodes(names.aux,css="td")),"href")
	output <- data.frame("Municipality"=names, "N firms"=n)
	return(output)
}