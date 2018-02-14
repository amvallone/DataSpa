#' @name data.firm
#' @rdname data.firm
#'
#' @title Collect firm information at muncipality level
#'
#' @description Generate a data frame of firms information of a particular municipality
#'
#' @param provincia one of the 52 Spanish provinces. See \link{getbase.pob} for details.
#'
#' @details is an interative function, require the selection of a particular municipality. 
#'
#' @return a data frame with the following variables:
#'
#' @examples
#' \dontrun{data.firm("Ceuta")}
#'
#' @export

data.firm<-function(provincia){
	prov<-toupper(provincia)
	b<-"http://www.axesor.es/directorio-informacion-empresas/empresas-de-"
	p<-c("albacete","alicante","almeria","alava","asturias","avila","aadajoz","baleares","barcelona", "vizcaya","burgos","caceres","cadiz","cantabria","castellon","ciudad-real","cordoba","la-coruna","cuenca","guipuzcoa","girona","granada","guadalajara","huelva","huesca","jaen","leon","lleida","lugo","madrid","malaga","murcia","navarra","orense","palencia","las-palmas","pontevedra","la-rioja","salamanca","santa-cruz-de-tenerife","segovia","sevilla","soria","tarragona","teruel","toledo","valencia","valladolid","zamora","zaragoza","ceuta","melilla")
	names(p)<-c("ALBACETE","ALICANTE","ALMERIA","ARABA","ASTURIAS","AVILA","BADAJOZ"," BALEARES","BARCELONA","BIZKAIA","BURGOS","CACERES","CADIZ","CANTABRIA","CASTELLO","CIUDAD REAL","CORDOBA","A CORU\u00D1A","CUENCA","GIPUZKOA","GIRONA","GRANADA","GUADALAJARA","HUELVA","HUESCA","JAEN","LEON","LLEIDA","LUGO","MADRID","MALAGA","MURCIA","NAVARRA","OURENSE","PALENCIA","LAS PALMAS","PONTEVEDRA", "LA RIOJA","SALAMANCA","TENERIFE","SEGOVIA","SEVILLA","SORIA","TARRAGONA","TERUEL","TOLEDO","VALENCIA","VALLADOLID","ZAMORA","ZARAGOZA","CEUTA","MELILLA")
	url<-paste(b,p[prov],sep="")
	mun<-municipio(url)
	cual<-nn.municipio(url)
	print(cual)
	resp<-readline("Indique el numero del muncipio de su interes")
	set<-mun[as.numeric(resp)]
	set
	lista<-lista.empresa(set)
	cat("se analizan",length(lista),"casos \n")
	salida<-(matrix(0,nrow=length(lista),ncol=21))
	for (i in 1:length(lista)){
		emp<-empresa(lista[i])
		if(dim(emp)[1]>1){
			salida[i,]<-emp[1]	
		} else {
			salida[i,]<-emp
		}
		cat("iter",i,"\n")
		#Sys.sleep(0.2)
	}
	salida
}


