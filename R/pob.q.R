#' @importFrom stringr str_trim
#' @name pob.q
#' @rdname pob.q
#'
#' @title  Population by quinquennials age group data
#' @description import into R the  population by quinquennials age group data
#'
#' @param year A numerical value between 1996 and the current year indicating the year of the required data.#' @param provincia one of the 52 Spain`s province.
#'
#' @return a list containing a total population data frame and the population grouped by sex. Each data frame contains the following variables: #' - \code{cod} is the municipality identification number based in the INE codification.#'	- \code{Name} the municipality name.#' - \code{Total} total municipality population#' - 21 variables containing the population by quinquennials age group
#'
#' @example
#' pob.q(2016,"Madrid")
#'
#' @export

pob.q<-function(year,provincia){
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
		file<-paste(paste("pob_q",year,provincia,sep="_"),".xlsx",sep="")
		if(sum(dir(dirc)==file)==0){
			getbase.pob(year,provincia)
		}
		abre<-paste(dirc,file,sep="")
		datos<-as.data.frame(readxl::read_excel(abre)) #no more Java dependencies!!!
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
		if (as.numeric(year)<2006){
			for (i in 1:d[1]){
			nn<-unlist(strsplit(nombres[i]," "))
			codigo[i]<-str_trim(nn[5])
			municipio[i]<-str_trim(nn[6])
			}
		} else {
			for (i in 1:d[1]){
			nn<-unlist(strsplit(nombres[i],"-"))
			codigo[i]<-str_trim(nn[1])
			municipio[i]<-str_trim(nn[2])
			}
		}
		salida<-cbind(codigo,municipio,edades)
		salida[,1]<-as.character(salida[,1])
		salida[,2]<-as.character(salida[,2])
		salida[,3:dim(salida)[2]]<-apply(salida[,3:dim(salida)[2]],2,as.numeric.factor)
		dd<-h-t-1
		s.t<-salida[t:(h-1),]
		s.h<-salida[h:(m-1),]
		s.m<-salida[m:(m+dd),]
		out<-list(s.t[-1,],s.h[-1,],s.m[-1,])
		names(out)<-c("Ambos Sexos","Hombre","Mujeres")
		out
}

