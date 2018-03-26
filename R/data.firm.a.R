#' @name data.firm.a
#' @rdname data.firm.a
#'
#' @title Collects information of self-employed at a municipality level
#'
#' @description \code{data.firm.a} generates a data frame of self-employment information of a particular municipality
#'
#' @param provincia one of the 52 Spanish provinces. See \link{getbase.pob} for details.
#'
#' @details It is an interactive function, which requires the selection of a particular municipality. 
#'
#' @return A data frame containing the following variables for each self-employment: location (province, municipality, address), company characteristics (name, birth, legal form, social object), economic activity codes and self-employment URL
#'
#' @family firm functions
#' @examples
#' \dontrun{data.firm.a("Ceuta")}
#'
#' @export

data.firm.a<-function(provincia){
	prov<-toupper(provincia)
	b <- "https://autonomos.axesor.es/informe-de-autonomo/provincias/"
	p<-c("Albacete","Alicante","Almeria","Alava","Asturias","Avila","Badajoz","Baleares","Barcelona", "Vizcaya","Burgos","Caceres","Cadiz","Cantabria","Castellon","Ciudad-Real","Cordoba","La-Coruna","Cuenca","Guipuzcoa","Girona","Granada","Guadalajara","Huelva","Huesca","Jaen","Leon","Lleida","Lugo","Madrid","Malaga","Murcia","Navarra","Orense","Palencia","Las-Palmas","Pontevedra","La-Rioja","Salamanca","Santa-Cruz-De-Tenerife","Segovia","Sevilla","Soria","Tarragona","Teruel","Toledo","Valencia","Valladolid","Zamora","Zaragoza","Ceuta","Melilla")
	names(p)<-c("ALBACETE","ALICANTE","ALMERIA","ARABA","ASTURIAS","AVILA","BADAJOZ"," BALEARES","BARCELONA","BIZKAIA","BURGOS","CACERES","CADIZ","CANTABRIA","CASTELLO","CIUDAD REAL","CORDOBA","A CORU\u00D1A","CUENCA","GIPUZKOA","GIRONA","GRANADA","GUADALAJARA","HUELVA","HUESCA","JAEN","LEON","LLEIDA","LUGO","MADRID","MALAGA","MURCIA","NAVARRA","OURENSE","PALENCIA","LAS PALMAS","PONTEVEDRA", "LA RIOJA","SALAMANCA","TENERIFE","SEGOVIA","SEVILLA","SORIA","TARRAGONA","TERUEL","TOLEDO","VALENCIA","VALLADOLID","ZAMORA","ZARAGOZA","CEUTA","MELILLA")
	url<-paste(b,p[prov],sep="")
	mun<-municipio.a(url)
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
	lista<-lista.empresa.a(set)
	cat("se analizan",length(lista),"casos \n")
	salida<-(matrix(0,nrow=length(lista),ncol=12))
	for (i in 1:length(lista)){
		emp<-empresa.a(lista[i])
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


