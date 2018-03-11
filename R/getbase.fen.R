#' @importFrom utils download.file capture.output read.table
#' @import stringr 
#' @import xlsx
#' @import rvest
#' @importFrom xml2 read_html
#' @importFrom stats na.omit
#' @importFrom XML htmlParse
#' @importFrom stringi stri_count
#'
#' @name getbase.fen
#' @rdname getbase.fen
#'
#' @title Collect information of municipality population phenomena.
#'
#' @description Download the population phenomena data of the province`s municipalities corresponding to the required. 
#'
#' @param year A numerical value between 1996 and the current year indicating the year of the required data.
#' @param provincia one of the 52 Spain’s province.
#'
#' @family download functions
#' @details You can use in capital lletter or not the names of spanish provincies. The full list of spanish provincies is:
#'	
#' "ALBACETE", "ALICANTE", "ALMERIA", "ARABA", "ASTURIAS", "AVILA", "BADAJOZ", "BALEARES", "BARCELONA", "BIZKAIA", "BURGOS", "CACERES", "CADIZ", "CANTABRIA", "CASTELLO", "CIUDAD REAL", "CORDOBA", "A CORUÑA", "CUENCA", "GIPUZKOA", "GIRONA", "GRANADA", "GUADALAJARA", "HUELVA", "HUESCA", "JAEN", "LEON", "LLEIDA", "LUGO", "MADRID", "MALAGA", "MURCIA", "NAVARRA", "OURENSE", "PALENCIA",  "LAS PALMAS", "PONTEVEDRA", "LA RIOJA", "SALAMANCA", "TENERIFE", "SEGOVIA", "SEVILLA","SORIA", "TARRAGONA", "TERUEL", "TOLEDO", "VALENCIA", "VALLADOLID", "ZAMORA", "ZARAGOZA", "CEUTA" and "MELILLA"
#'
#' @return a \code{xlsx} file saved in \code{data_poblacion} folder into the working directory wtih default name \code{fen_year_provincia}. 
#' @examples
#' getbase.fen(2005,"Madrid")
#'
#' @export


getbase.fen<-function(year,provincia){
		year<-as.character(year)
		provincia<-toupper(provincia)
		provincia<-a.letter(provincia)
		if(dir.exists(file.path(getwd(),"data_poblacion"))==FALSE){
		dir.create(file.path(getwd(),"data_poblacion"))
		}
		file<-paste(getwd(),"/data_poblacion/",paste("fen",year,provincia,sep="_"),".xlsx",sep="")
		p<-c("02","03","04","01","33","05","06","07","08","48","09","10","11","39","12","13","14","15","16","20","17","18","19","21","22","23","24","25","27","28","29","30","31","32","34","35","36","26","37","38","40","41","42","43","44","45","46","47","49","50","51","52")
		names(p) <- c("ALBACETE","ALICANTE","ALMERIA","ARABA","ASTURIAS","AVILA","BADAJOZ","BALEARES","BARCELONA","BIZKAIA","BURGOS","CACERES","CADIZ","CANTABRIA","CASTELLO","CIUDAD_REAL","CORDOBA","A_CORUNA","CUENCA","GIPUZKOA","GIRONA","GRANADA","GUADALAJARA","HUELVA","HUESCA","JAEN","LEON","LLEIDA","LUGO","MADRID","MALAGA","MURCIA","NAVARRA","OURENSE","PALENCIA","LAS_PALMAS","PONTEVEDRA","LA_RIOJA","SALAMANCA","TENERIFE","SEGOVIA","SEVILLA","SORIA","TARRAGONA","TERUEL","TOLEDO","VALENCIA","VALLADOLID","ZAMORA","ZARAGOZA","CEUTA","MELILLA")
		n<-p[provincia]
		url<-paste("http://www.ine.es/jaxi/files/_px/es/xlsx/t20/e301/fenom/a",year,"/l0/","230",n,".px?nocab=1",sep="")
		download.file(url,file, mode='wb')
}

