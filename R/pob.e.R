#' @name pob.e
#' @rdname pob.e
#'
#' @title  Population grouped by nationality and sex data
#' @description import into R the  population grouped by nationality and sex data
#'
#' @param year A numerical value between 1996 and the current year indicating the year of the required data.
#' @param provincia one of the 52 Spain`s province.
#'
#' @return a list containing a total population data frame and the population grouped by sex. Each data frame contains the following variables: 
#' \itemize{
#' 		\item \code{cod} is the municipality identification number based in the INE codification.
#'		\item \code{Name} the municipality name.
#' 		\item \code{Total} total municipality population
#' 		\item Three variables containing the population divided into Younger than 16 years old, Between 16 and 64 years old and Older than 65 years old
#'	}
#'
#' @example
#' pob.e(2016,"Madrid")
#'
#' @export

pob.e<-function(year,provincia){
		year<-as.character(year)
		provincia<-toupper(provincia)
		prov<-provincia
		if(str_detect(provincia," ")==TRUE){
		provincia<-str_replace_all(provincia," ","_")
		}
	    if(str_detect(provincia,"Ñ")==TRUE){
		provincia<-str_replace_all(provincia,"Ñ","N")
		}
		dirc<-paste(getwd(),"/data_poblacion/",sep="")
		file<-paste(paste("pob_e",year,provincia,sep="_"),".xlsx",sep="")
		if(sum(dir(dirc)==file)==0){
			getbase.pob(year,provincia,extr=TRUE)
		}
		abre<-paste(dirc,file,sep="")
		datos<-as.data.frame(readxl::read_excel(abre))
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
		fila<-c("Cod","Municipio","Total","Total Menores de 16 años","Total De 16 a 64 años","Total De 65 y más años", "Total Esp","Esp Menores de 16 años","Esp De 16 a 64 años","Esp De 65 y más años","Total Extr","Extr Menores de 16 años","Extr De 16 a 64 años","Extr De 65 y más años")
		colnames(salida)<-fila
		dd<-h-t-1
		s.t<-salida[t:(h-1),]
		s.h<-salida[h:(m-1),]
		s.m<-salida[m:(m+dd),]
		out<-list(s.t[-1,],s.h[-1,],s.m[-1.])
		names(out)<-c("Ambos Sexos","Hombre","Mujeres")
		out
}
