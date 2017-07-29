#' @name getbase.pob
#' @rdname getbase.pob
#'
#' @title Collect information of municipality population.
#'
#' @description Download the population data of the province`s municipalities corresponding to the required. 
#'
#' @param year A numerical value between 1996 and the current year indicating the year of the required data.
#' @param provincia one of the 52 Spain’s province.
#' @param extr Logical variable, is foreign population? FALSE is the default value.
#'
#' @return a \code{xlsx} file saved in \code{data_poblacion} folder into the working directory wtih default name \code{pob_q_year_provincia.xls}.If \code{extr} is TRUE, the file will be saved as \code{pob_e_year_provincia.xls}. In case of \code{anual} is TRUE the file is saved by the name \code{pob_a_year_provincia.xls}
#' 
#' @examples
#' getbase.pob(2005,”Madrid”)
#'
#' @export


getbase.pob<-function(year,provincia,extr=FALSE,anual=FALSE){
		year<-as.character(year)
		if(extr==TRUE && anual==TRUE) stop("No existe datos para estos casos")
		if(anual==TRUE && as.numeric(year)<2011) stop("No existe datos para estos casos")
		provincia<-toupper(provincia)
		prov<-provincia
		if(str_detect(provincia," ")==TRUE){
		provincia<-str_replace_all(provincia," ","_")
		}
	    if(str_detect(provincia,"Ñ")==TRUE){
		provincia<-str_replace_all(provincia,"Ñ","N")
		}
		if(dir.exists(file.path(getwd(),"data_poblacion"))==FALSE){
		dir.create(file.path(getwd(),"data_poblacion"))
		}
		if (extr==TRUE) {
			if(as.numeric(year)<2002) {q<-4} else {q<-2}
			e<-"e"
			} else {
			q<-1
			e<-"q"
			}
		if (anual==TRUE) { q<-6; e<-"a"}
		file<-paste(getwd(),"/data_poblacion/",paste("pob",e,year,provincia,sep="_"),".xlsx",sep="")
		p<-c("02","03","04","01","33","05","06","07","08","48","09","10","11","39","12","13","14","15","16","20","17","18","19","21","22","23","24","25","27","28","29","30","31","32","34","35","36","26","37","38","40","41","42","43","44","45","46","47","49","50","51","52")
		names(p)<-c("ALBACETE","ALICANTE","ALMERIA","ARABA","ASTURIAS","AVILA","BADAJOZ"," BALEARES","BARCELONA","BIZKAIA","BURGOS","CACERES","CADIZ","CANTABRIA","CASTELLO","CIUDAD REAL","CORDOBA","A CORUÑA","CUENCA","GIPUZKOA","GIRONA","GRANADA","GUADALAJARA","HUELVA","HUESCA","JAEN","LEON","LLEIDA","LUGO","MADRID","MALAGA","MURCIA","NAVARRA","OURENSE","PALENCIA","LAS PALMAS","PONTEVEDRA", "LA RIOJA","SALAMANCA","TENERIFE","SEGOVIA","SEVILLA","SORIA","TARRAGONA","TERUEL","TOLEDO","VALENCIA","VALLADOLID","ZAMORA","ZARAGOZA","CEUTA","MELILLA")
		n<-p[prov]
		url<-paste("http://www.ine.es/jaxi/files/_px/es/xlsx/t20/e245/p05/a",year,"/l0/000",n,"00",q,".px?nocab=1",sep="")
		download.file(url,file)
}