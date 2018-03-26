#' @name pob.n.tot
#' @rdname pob.n.tot
#'
#' @title  National Population  data
#' @description \code{pob.n.tot} imports into an R file the national population of Spain by municipality. 
#'
#' @param year a numerical value from 1996 and the latest available year, which indicates the year of the required database.
#' @param provincia one of the 52 Spainish provinces.
#'
#' @return It is a data frame containing the municipality national population of Spain. 
#'
#' @family Loading functions
#' @examples
#' pob.n.tot(2016,"Madrid")
#'
#' @export


pob.n.tot<-function(year,provincia){
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
		datos<-xlsx::read.xlsx(abre,1,colIndex=c(1,2,6,7), encoding ="UTF-8")
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
		edades<-datos[,2:d[2]]
		salida<-cbind(codigo,municipio,edades)
		salida[,1]<-as.character(salida[,1])
		salida[,2]<-as.character(salida[,2])
		salida[,3:5]<-apply(salida[,3:5],2,as.numeric.factor)
		fila<-c("Cod","Municipio","Total")
		colnames(salida)<-fila
		if (as.numeric(year)<2002){
			s.t<-salida[t:(h-1),1:3]
		} else {
			s.t<-salida[t:(h-1),c(1,2,4)]
		}
		s.t<-s.t[-1,]
		s.t[1,2]<-"Todos"
		s.t
		
}
