#' @name getbase.paro
#' @rdname getbase.paro
#'
#' @title Collect information of municipality unemploymet.
#'
#' @description Download the work force data of the province`s municipalities corresponding to the required. 
#'
#' @param year A numerical value between 2005 and the current year indicating the year of the required data.
#' @param mes one of the 12 moths in Spanish indicating the moth when the data was collected.#' @param provincia one of the 52 Spain’s province.#'
#' @return a file host in \code{data_paro} folder into the working directory wtih name \code{paro_MUNI_provincia_mmyy.xls}.
#' 
#' @examples
#' getbase.paro(2005,”julio”,”Madrid”)
#'
#' @export

getbase.paro<-function(year,mes,provincia){
	provincia<-toupper(provincia)
	year<-as.character(year)
	if(dir.exists(file.path(getwd(),"data_paro"))==FALSE){
		dir.create(file.path(getwd(),"data_paro"))
		}
	provincia<-toupper(provincia)
	if(str_detect(provincia," ")==TRUE){
		provincia<-str_replace_all(provincia," ","_")
		}
	if(str_detect(provincia,"Ñ")==TRUE){
		provincia<-str_replace_all(provincia,"Ñ","N")
		}
	mes<-tolower(mes)
	nn.mes<-seq(1,12,1)
	names(nn.mes)<-c("enero","febrero","marzo","abril","mayo","junio","julio","agosto","septiembre","octubre","noviembre","diciembre")
	cod<-paste("0",nn.mes[mes],substr(year,3,4),sep="")
	name<-paste(paste("MUNI",provincia,cod,sep="_"),".xls",sep="")
	url<-paste("http://www.sepe.es/contenidos/que_es_el_sepe/estadisticas/datos_estadisticos/municipios/",year,"/",paste(mes,year,sep="_"),"/",name,sep="")
	dir<-paste(getwd(),"/data_paro/",sep="")
	file<-paste(dir,"paro_",name,sep="")
	download.file(url,file)
}
