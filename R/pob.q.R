
#' @name pob.q
#' @rdname pob.q
#'
#' @title Population by five-year age groups and sex
#' @description \code{pob.q} imports into an R file the Spanish municipality population by five-year age groups and sex
#'
#' @param year a numerical value from 1996 and the latest available year, which indicates the year of the required database.#' @param provincia one of the 52 Spainish provinces.
#'
#' @return It is a list containing a data frame of the Spanish municipality population by five-year age groups and sex. Each data frame contains the following variables:: #' - \code{cod} is the municipality identification number based in the INE codification.#'	- \code{Name} is the municipality name.#' - \code{Total} is the municipality population.#' - Twenty-one variables containing the population by five-year age groups.
#'
#' @family Loading functions
#' @examples
#' pob.q(2016,"Caceres")
#'
#' @export

pob.q<-function(year,provincia){
		year<-as.character(year)
		provincia<-toupper(provincia)
		prov<-provincia
		provincia<-a.letter(provincia)
		dirc<-paste(getwd(),"/data_poblacion/",sep="")
		file<-paste(paste("pob_q",year,provincia,sep="_"),".xlsx",sep="")
		if(sum(dir(dirc)==file)==0){
			getbase.pob(year,provincia)
		}
		abre<-paste(dirc,file,sep="")
		#datos<-xlsx::read.xlsx(abre,1, encoding ="UTF-8")
		datos<-as.data.frame(readxl::read_excel(abre))
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
		s.t<-salida[(t+1):(h-1),]
		s.h<-salida[(h+1):(m-1),]
		s.m<-salida[(m+1):(m+dd),]
		out<-list(s.t[-1,],s.h[-1,],s.m[-1,])
		names(out)<-c("Ambos Sexos","Hombre","Mujeres")
		out
}

