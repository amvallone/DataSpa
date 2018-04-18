#' @name pob.e.tot
#' @rdname pob.e.tot
#'
#' @title  Total Foreign Population
#' @description \code{pob.e.tot} imports into an R file the municipality total foreign population. 
#'
#' @param year a numerical value from 1996 and the latest available year, which indicates the year of the required database.
#' @param provincia one of the 52 Spainish provinces.
#'
#' @return It is a data frame containing the municipality total foreign population 
#'
#' @family Loading functions
#' @examples
#' pob.e.tot(2016,"Madrid")
#'
#' @export

pob.e.tot<-function(year,provincia){
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
		datos<-xlsx::read.xlsx(abre,1,colIndex=c(1,4,5,10,11), encoding ="UTF-8")
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
			salida[,3:dim(salida)[2]]<-apply(salida[,3:dim(salida)[2]],2,as.numeric.factor)
			salida[which(is.na(salida[,3])),3]<-0
			salida[which(is.na(salida[,4])),4]<-0
			salida[which(is.na(salida[,5])),5]<-0
			salida[which(is.na(salida[,6])),6]<-0
			fila<-c("Cod","Municipio","Total")
			colnames(salida)<-fila
			if(as.numeric(year)<2002){
				s.t<-salida[t:(h-1),1:3]
			} else {
				s.t<-salida[t:(h-1),c(1,2,5)]
			}
		s.t<-s.t[-c(1:2),]
		#s.t[1,2]<-"Todos"
		s.t
}
