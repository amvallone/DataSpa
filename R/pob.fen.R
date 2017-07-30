#' @importFrom stats complete.cases
#' @name pob.fen
#' @rdname pob.fen
#'
#' @title  Principal municiplaity puplation phenomena
#' @description import into R the principal population phenomena 
#'
#' @param year A numerical value between 1996 and the current year indicating the year of the required data.
#' @param provincia one of the 52 Spain`s province.
#'
#' @return a  data frame conatining the principal population phenomena 
#'	\itemize{
#'		\item \code{Birth} number of birth in the municipality#'		\item \code{Deaths} Number of death in the municipality 
#'	}
#'
#' @example
#' pob.fen(2016,"Madrid")
#'
#' @export


pob.fen<-function(year,provincia){
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
		file<-paste(paste("fen",year,provincia,sep="_"),".xlsx",sep="")
		if(sum(dir(dirc)==file)==0){
			getbase.fen(year,provincia)
		}
		abre<-paste(dirc,file,sep="")
		datos<-as.data.frame(readxl::read_excel(abre,range=cell_cols(c(1,2,5))))
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
		ids<-as.data.frame(cbind(codigo,municipio))
		salida<-cbind(ids,cifras)
		colnames(salida)<-c("Cod","Municipio","Nacidos","Fallecidos")
		salida
}
