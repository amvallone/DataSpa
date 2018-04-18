#' @name pob.m.tot
#' @rdname pob.m.tot
#'
#' @title  Female Population  data
#' @description \code{pob.m.tot} imports into an R file the female population of Spain by municipality.
#'
#' @param year a numerical value from 1996 and the latest available year, which indicates the year of the required database.
#' @param provincia one of the 52 Spainish provinces.
#'
#' @return  It is a  data frame conatining the municipality total female population 
#'
#' @family Loading functions
#' @examples
#' pob.m.tot(2016,"Madrid")
#'
#' @export




pob.m.tot<-function(year,provincia){
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
		dd<-h-t-1
		s.m<-salida[m:(m+dd),1:3]
		s.m<-s.m[-c(1:2),]
		#s.m[1,2]<-"Todos"
		s.m
}

