#' @importFrom stats complete.cases
#' @name pob.fen
#' @rdname pob.fen
#'
#' @title  Number of births and deaths at a municipality level
#' @description \code{pob.fen} imports into an R file two main demographic indexes at the municipality level: number of births and deaths. 
#'
#' @param year a numerical value from 1996 and 2015, which indicates the year of the required database
#' @param provincia one of the 52 Spainish province.
#'
#' @return It is a data frame containing two main demographic indexes: 
#'	\itemize{
#'		\item \code{Birth} number of birth in the municipality#'		\item \code{Deaths} Number of death in the municipality 
#'	}
#'
#' @family Loading functions
#' @examples
#' pob.fen(2012,"Madrid")
#'
#' @export


pob.fen<-function(year,provincia){
		year<-as.character(year)
		provincia<-toupper(provincia)
		prov<-provincia
		provincia<-a.letter(provincia)
		dirc<-paste(getwd(),"/data_poblacion/",sep="")
		file<-paste(paste("fen",year,provincia,sep="_"),".xlsx",sep="")
		if(sum(dir(dirc)==file)==0){
			getbase.fen(year,provincia)
		}
		abre<-paste(dirc,file,sep="")
		datos<-xlsx::read.xlsx(abre,1,colIndex=c(1,2,5), encoding ="UTF-8")
		datos<-datos[which(complete.cases(datos)==TRUE),]
		datos<-datos[-1,]
		d<-dim(datos)
		nombres<-as.character(datos[,1])
		codigo<-rep("AA",d[1])
		municipio<-rep("AA",d[1])
		for (i in 1:d[1]){
			nn<-unlist(strsplit(nombres[i]," "))
			codigo[i]<-str_trim(nn[1])
			if(length(nn)>2){
				nom<-paste0(nn[2:length(nn)],collapse=" ")
				municipio[i]<-nom
			} else {
			municipio[i]<-str_trim(nn[2])
			}
		}
		cifras<-as.data.frame(datos[,2:3])
		cifras<-apply(cifras,2,as.numeric.factor)
		if (is.null(dim(cifras))){ cifras <- t(as.matrix(cifras)) }
		ids<-as.data.frame(cbind(codigo,municipio))
		salida<-cbind(ids,cifras)
		colnames(salida)<-c("Cod","Municipio","Nacidos","Fallecidos")
		salida
}
