#' @name getbase.paro
#' @rdname getbase.paro
#'
#' @title Collects information of unemployed at a municipality level.
#'
#' @description \code{getbase.paro} downloads data about the unemployed of the Spanish municipalities by province.
#'
#' @param year a numerical value between 2005 and the last available, which indicates the year of the required database.
#' @param mes oone of the 12 months –in Spanish language– indicating the month of the data collection.#' @param provincia one of the 52 Spainish provinces.#'
#' @return It is a \code{xlsx} file host in the \code{data_paro} folder which, in turn, is located inside the working directory called  \code{paro_MUNI_provincia_mmyy.xls}.
#' 
#' @examples
#' getbase.paro(2005,"julio","Madrid")
#'
#' @details The month must be called as follows: "enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre" and "diciembre".
#' 
#'	The names of the Spanish provinces may or may not be called. If yes, employ capital letters as follows:
#'
#' "ALBACETE", "ALICANTE", "ALMERIA", "ARABA", "ASTURIAS", "AVILA", "BADAJOZ", " BALEARES", "BARCELONA", "BIZKAIA", "BURGOS", "CACERES", "CADIZ", "CANTABRIA", "CASTELLON", "CIUDAD REAL", "CORDOBA", "A CORUÑA", "CUENCA", "GIPUZKOA", "GIRONA", "GRANADA", "GUADALAJARA", "HUELVA", "HUESCA", "JAEN", "LEON", "LLEIDA", "LUGO", "MADRID", "MALAGA", "MURCIA", "NAVARRA", "OURENSE", "PALENCIA",  "LAS PALMAS", "PONTEVEDRA", "LA RIOJA", "SALAMANCA", "TENERIFE", "SEGOVIA", "SEVILLA","SORIA", "TARRAGONA", "TERUEL", "TOLEDO", "VALENCIA", "VALLADOLID", "ZAMORA", "ZARAGOZA", "CEUTA" and "MELILLA"
#'
#' @family download functions
#' @export

getbase.paro<-function(year,mes,provincia){
	year<-as.character(year)
	if(dir.exists(file.path(getwd(),"data_paro"))==FALSE){
		dir.create(file.path(getwd(),"data_paro"))
		}
	provincia<-toupper(provincia)
	provincia<-a.letter(provincia)
	mes<-tolower(mes)
	nn.mes<-seq(1,12,1)
	names(nn.mes)<-c("enero","febrero","marzo","abril","mayo","junio","julio","agosto","septiembre","octubre","noviembre","diciembre")
	cod<-paste("0",nn.mes[mes],substr(year,3,4),sep="")
	name<-paste(paste("MUNI",provincia,cod,sep="_"),".xls",sep="")
	url<-paste("http://www.sepe.es/contenidos/que_es_el_sepe/estadisticas/datos_estadisticos/municipios/",year,"/",paste(mes,year,sep="_"),"/",name,sep="")
	dir<-paste(getwd(),"/data_paro/",sep="")
	file<-paste(dir,"paro_",name,sep="")
	download.file(url,file, mode='wb')
}
