#' @name pob.tot
#' @rdname pob.tot
#'
#' @title Population  data
#' @description \code{pob.tot} imports into an R file the total population of Spain by municipality. 
#'
#' @param year a numerical value from 1996 and the latest available year, which indicates the year of the required database
#' @param provincia one of the 52 Spainish provinces.
#'
#' @return It is a data frame containing the total population of Spain at the municipality level.
#'
#' @family Loading functions
#' @examples
#' pob.tot(2016,"Madrid")
#'
#' @export

pob.tot<-function(year,provincia){
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
		datos<-xlsx::read.xlsx(abre,1,colIndex=c(1:4), encoding ="UTF-8")
		d<-dim(datos)
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
		t<-which(datos[,1]=="Ambos sexos")
		h<-which(datos[,1]=="Hombres")
			if(sum(h)==0) {h<-which(datos[,1]=="Varones")}
		m<-which(datos[,1]=="Mujeres")
		edades<-datos[,2:d[2]]
		salida<-cbind(codigo,municipio,edades)
		salida[,1]<-as.character(salida[,1])
		salida[,2]<-as.character(salida[,2])
		salida[,3:4]<-apply(salida[,3:4],2,as.numeric.factor)
		fila<-c("Cod","Municipio","Total")
		colnames(salida)<-fila
		s.t<-salida[t:(h-1),1:3]
		s.t<-s.t[-c(1:2),]
		#s.t[1,2]<-"Todos"
		s.t
}
