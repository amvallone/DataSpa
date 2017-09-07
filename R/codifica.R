#' name codifica
#'
#' @title Imputa los codigos del INE a los muncipios
#'
#' @param x un character con el nombre a buscar
#' @param provincia una provincia de Spain
#'
#' @value el codigo INE del municipaio sacado de la lista posibles nombres
#' @export

codifica<-function(x,provincia){
			p<-c("02","03","04","01","33","05","06","07","08","48","09","10","11","39","12","13","14","15","16","20","17","18","19","21","22","23","24","25","27","28","29","30","31","32","34","35","36","26","37","38","40","41","42","43","44","45","46","47","49","50","51","52")
		names(p)<-c("ALBACETE","ALICANTE","ALMERIA","ARABA","ASTURIAS","AVILA","BADAJOZ"," BALEARES","BARCELONA","BIZKAIA","BURGOS","CACERES","CADIZ","CANTABRIA","CASTELLO","CIUDAD REAL","CORDOBA","A CORU\u00D1A","CUENCA","GIPUZKOA","GIRONA","GRANADA","GUADALAJARA","HUELVA","HUESCA","JAEN","LEON","LLEIDA","LUGO","MADRID","MALAGA","MURCIA","NAVARRA","OURENSE","PALENCIA","LAS PALMAS","PONTEVEDRA", "LA RIOJA","SALAMANCA","TENERIFE","SEGOVIA","SEVILLA","SORIA","TARRAGONA","TERUEL","TOLEDO","VALENCIA","VALLADOLID","ZAMORA","ZARAGOZA","CEUTA","MELILLA")
		buscar<-subset(mun,mun[,1]==p[provincia])
		x<-simpleCap(x)
		e<-agrep(x,buscar[,4])
		salida<-buscar[e,]
		if (length(e)>1){
			s<-subset(buscar[e,],buscar[e,4]==x)
			if (dim(s)[1]==0){
				salida<-salida[which(duplicated(salida[,2])==TRUE),]
			} else{
				salida<-s
			}
		}
		if (dim(salida)[1]==0){
			x1<-unlist(strsplit(x, " "))
			d<-sapply(x1,nchar)
			e1<-agrep(x1[which(d==max(d))],buscar[,4])
			salida<-buscar[e1,]
			if (length(e1)>1){salida<-salida[which(duplicated(salida[,2])==TRUE),]}		
			}
		cod<-paste(salida[,1],salida[,3],sep="")
		if(length(cod)==0){ 
			warning("there are same municipalities without cod")
			cod<-"No macth found" 
			}
		return(cod)
	}