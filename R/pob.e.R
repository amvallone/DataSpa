#' @name pob.e
#' @rdname pob.e
#'
#' @title  Population grouped by nationality and sex
#' @description \code{pob.e} imports into R the municipality population grouped by nationality and sexa
#'
#' @param year a numerical value from 1996 and the latest available, which indicates the year of the required database.
#' @param provincia one of the 52 Spainish provinces.
#'
#' @return It is a list containing a data frame with total population and population by sex. Each data frame contains the following variables: 
#' \itemize{
#' 		\item \code{cod} is the municipality identification number based in the INE codification.
#'		\item \code{Name} is the municipality name.
#' 		\item \code{Total} is the municipality population.
#' 		\item Three variables containing information about population by age (younger than 16 years old, from 16 to 64 years old and older than 65 years old), which are, in turn, grouped by total population, foreign population and national population.
#'	}
#'
#' @family Loading functions
#' @examples
#' pob.e(2016,"Madrid")
#'
#' @export

pob.e<-function(year,provincia){
		year<-as.character(year)
		provincia<-toupper(provincia)
		prov<-provincia
		provincia<-a.letter(provincia)
		dirc<-paste(getwd(),"/data_poblacion/",sep="")
		file<-paste(paste("pob_e",year,provincia,sep="_"),".xlsx",sep="")
		if(sum(dir(dirc)==file)==0){
			getbase.pob(year,provincia,extr=TRUE)
		}
		abre<-paste(dirc,file,sep="")
		datos<-xlsx::read.xlsx(abre,1, encoding ="UTF-8")
		d<-dim(datos)
		t<-which(datos[,1]=="Ambos sexos")
		h<-which(datos[,1]=="Hombres")
			if(sum(h)==0) {h<-which(datos[,1]=="Varones")}
		m<-which(datos[,1]=="Mujeres")
		edades<-datos[,2:d[2]]
		nombres<-as.character(datos[,1])
		codigo<-rep("AA",d[1])
		municipio<-rep("AA",d[1])
		for (i in 1:d[1]){
			nn<-unlist(strsplit(nombres[i],"-"))
			codigo[i]<-nn[1]
			municipio[i]<-nn[2]
		}
		salida<-cbind(codigo,municipio,edades)
		salida[,1]<-as.character(salida[,1])
		salida[,2]<-as.character(salida[,2])
		salida[,3:dim(salida)[2]]<-apply(salida[,3:dim(salida)[2]],2,as.numeric.factor)
		fila<-c("Cod","Municipio","Total","Total Menores de 16 a\u00F1os","Total De 16 a 64 a\u00F1os","Total De 65 y mas a\u00F1os", "Total Esp","Esp Menores de 16 a\u00F1os","Esp De 16 a 64 a\u00F1os","Esp De 65 y mas a\u00F1os","Total Extr","Extr Menores de 16 a\u00F1os","Extr De 16 a 64 a\u00F1os","Extr De 65 y mas a\u00F1os")
		colnames(salida)<-fila
		dd<-h-t-1
		s.t<-salida[(t+1):(h-1),]
		s.h<-salida[(h+1):(m-1),]
		s.m<-salida[(m+1):(m+dd),]
		out<-list(s.t[-1,],s.h[-1,],s.m[-1.])
		names(out)<-c("Ambos Sexos","Hombre","Mujeres")
		out
}
