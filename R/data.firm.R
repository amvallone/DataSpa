#' @import pbapply
#' @name data.firm
#' @rdname data.firm
#'
#' @title Collects information of firms at a municipality level
#'
#' @description Generate a data frame of firm information of a particular municipality
#'
#' @param provincia one of the 52 Spanish provinces. See \link{getbase.pob} for details.
#'
#' @details It is an interactive function, which requires the selection of a particular municipality. 
#'
#' @return A data frame containing the following variables for each company: location (province, municipality, address, geographic coordinates), company characteristics (name, birth, legal form, social object), main figures (number of employees, social capital, sales), economic activity codes and firm URL
#'
#' @family firm functions
#' @examples
#' \dontrun{data.firm("Ceuta")}
#'
#' @export

data.firm<-function(provincia){
	prov<-toupper(provincia)
	b<-"http://www.axesor.es/directorio-informacion-empresas/empresas-de-"
	p<-c("albacete","alicante","almeria","alava","asturias","avila","badajoz","baleares","barcelona", "vizcaya","burgos","caceres","cadiz","cantabria","castellon","ciudad-real","cordoba","la-coruna","cuenca","guipuzcoa","girona","granada","guadalajara","huelva","huesca","jaen","leon","lleida","lugo","madrid","malaga","murcia","navarra","orense","palencia","las-palmas","pontevedra","la-rioja","salamanca","santa-cruz-de-tenerife","segovia","sevilla","soria","tarragona","teruel","toledo","valencia","valladolid","zamora","zaragoza","ceuta","melilla")
	names(p)<-c("ALBACETE","ALICANTE","ALMERIA","ARABA","ASTURIAS","AVILA","BADAJOZ","BALEARES","BARCELONA","BIZKAIA","BURGOS","CACERES","CADIZ","CANTABRIA","CASTELLON","CIUDAD REAL","CORDOBA","A CORU\u00D1A","CUENCA","GIPUZKOA","GIRONA","GRANADA","GUADALAJARA","HUELVA","HUESCA","JAEN","LEON","LLEIDA","LUGO","MADRID","MALAGA","MURCIA","NAVARRA","OURENSE","PALENCIA","LAS PALMAS","PONTEVEDRA", "LA RIOJA","SALAMANCA","TENERIFE","SEGOVIA","SEVILLA","SORIA","TARRAGONA","TERUEL","TOLEDO","VALENCIA","VALLADOLID","ZAMORA","ZARAGOZA","CEUTA","MELILLA")
	url<-paste(b,p[prov],sep="")
	mun<-municipio(url)
	cual<-nn.municipio(url)
	display<-c(cual, paste("[",length(cual)+1,"] Todos",sep=""))
	cat(display, fill=FALSE)
	resp<-readline("Indique el numero del muncipio de su interes: ")
	if ((length(cual)+1)==as.numeric(resp)){
		resp <- seq_along(cual)
	} else {
		resp <- as.integer(unlist(strsplit(resp," ")))
	}
	set<-mun[resp]
	set
	lista<-unlist(sapply(set,lista.empresa,USE.NAMES=FALSE))
	cat("se analizan",length(lista),"casos \n")
	pp<-pbapply::pblapply(lista,empresa)
	salida <- do.call(rbind.data.frame,pp)
	return(salida)
}


