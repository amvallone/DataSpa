#' @name pob.a
#' @rdname pob.a
#'
#' @title  Population grouped by age data
#' @description \code{pob.a} imports into an R file the municipality population database grouped by age
#'
#' @param year a numerical value from 1996 and the last available, which indicates the year of the required database.
#' @param provincia one of the 52 Spainish provinces.
#'
#' @return It is a list containing a total population data frame and the population grouped by sex. Each data frame contains the following variables:: 
#'
#' \itemize{
#' 		\item \code{cod} is the municipality identification number based in the INE codification.
#'		\item \code{Name} the municipality name.
#' 		\item \code{Total} total municipality population
#' 		\item A hundred and one variables containing the population grouped by one-year age
#'	}
#'
#' @family Loading functions
#' @examples
#' pob.a(2016,"Caceres")
#'
#' @export


pob.a<-function(year,provincia){
		year<-as.character(year)
		if(as.numeric(year)<2011) stop("No existe datos para estos casos")
		provincia<-toupper(provincia)
		prov<-provincia
		provincia<-a.letter(provincia)
		dirc<-paste(getwd(),"/data_poblacion/",sep="")
		file<-paste(paste("pob_a",year,provincia,sep="_"),".xlsx",sep="")
		if(sum(dir(dirc)==file)==0){
			getbase.pob(year,provincia,anual=TRUE)
		}
		abre<-paste(dirc,file,sep="")
		datos<-xlsx::read.xlsx(abre,1, encoding ="UTF-8")
		d<-dim(datos)
		t<-which(datos[,1]=="Ambos sexos")
		h<-which(datos[,1]=="Hombres")
			if(sum(h)==0) {h<-which(datos[,1]=="Varones")}
		m<-which(datos[,1]=="Mujeres")
		edades<-datos[,2:d[2]]
		nn<-edades[which(edades[,1]=="Total"),]
		nn<-apply(nn,1,as.character)
		colnames(edades)<-nn
		nombres<-as.character(datos[,1])
		codigo<-rep("AA",d[1])
		municipio<-rep("AA",d[1])
		for (i in 1:d[1]){
			nn<-unlist(strsplit(nombres[i],"-"))
			codigo[i]<-str_trim(nn[1])
			municipio[i]<-str_trim(nn[2])
		}
		salida<-cbind(codigo,municipio,edades)
		salida[,1]<-as.character(salida[,1])
		salida[,2]<-as.character(salida[,2])
		salida[,3:dim(salida)[2]]<-apply(salida[,3:dim(salida)[2]],2,as.numeric.factor)
		dd<-h-t-1
		s.t<-salida[(t+1):(h-1),]
		s.h<-salida[(h+1):(m-1),]
		s.m<-salida[(m+1):(m+dd),]
		out<-list(s.t[-1,],s.h[-1,],s.m[-1,])
		names(out)<-c("Ambos Sexos","Hombre","Mujeres")
		out
}
