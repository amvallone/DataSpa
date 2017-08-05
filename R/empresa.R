#' @name empresa
#' @rdname empresa
#'
#' @title Extract fimr`s information
#'
#' @description Extract the firm's information from the Axesor webpage.
#'
#' @param http caracther with the Axesor firm URL
#'
#' @return A data frame
#'
#' @details for variable contained in the data frame see XX 
#'
#' @examples
#' empresa("http://www.axesor.es/Informes-Empresas/5774886/ABRELATAS_COMUNICACION_SL.html")
#'
#' @export 



empresa<-function(http){
			pg<-read_html(http)
			Nombre<-str_replace_all(html_text(html_node(pg,css="h3")), "([\n\r\t])", "")
			p1<-html_children(html_nodes(html_node(pg,css=".section-content"),css="dl"))
			Forma_juridica<-html_text(p1[grep("Forma jur\u00EDdica",capture.output(p1))],trim=TRUE)
			Tiempo_contituida<-sub("Constituida hace.","",str_replace_all(html_text(p1[grep("Constituida hace",capture.output(p1))],trim=TRUE), "([\n\r\t])", ""))
			if(length(Tiempo_contituida)==0) {Tiempo_contituida<-"N.I"}
			Dir<-html_text(html_children(html_children(p1[grep("Direcci\u00F3n de",capture.output(p1))-1])))
			if (length(Dir)==0) {Dir<-html_text(html_nodes(p1[grep("Direcci\u00F3n",capture.output(p1))],css="span"))}
			Direccion<-str_trim(str_replace_all(Dir[1], "([\n\r\t,])", ""))
			Cod_postal<-str_trim(str_replace_all(Dir[2], "([\n\r\t,])", ""))
			Mun<-simpleCap(str_trim(str_replace_all(Dir[3], "([\n\r\t,])", "")))
			Prov<-simpleCap(str_trim(str_replace_all(Dir[4], "([\n\r\t])", "")))
			Objetivo_social<-str_replace_all(html_text(html_nodes(p1[grep("Objeto social",capture.output(p1))],css="span")), "([\n\r\t])", "")
			if (length(Objetivo_social)>1) {Objetivo_social<-paste(Objetivo_social[1],Objetivo_social[2],sep="")}
			if(length(Objetivo_social)==0) {Objetivo_social<-"N.I"}
			C.A.E<-html_text(p1[grep("CNAE",capture.output(p1))])
			CodCAE<-na.omit(as.numeric(unlist(strsplit(C.A.E,"[^[:digit:]]"))))[1]
			DescCAE<-sub("[[:digit:]]+","",str_replace_all(C.A.E, "([\n\r\t])", ""))
			if(length(DescCAE)==0){ DescCAE<-NA}
			S.I.C<-html_text(p1[grep("SIC",capture.output(p1))])
			CodSIC<-na.omit(as.numeric(unlist(strsplit(S.I.C,"[^[:digit:]]"))))[1]
			DescSIC<-sub("[[:digit:]]+","",str_replace_all(S.I.C, "([\n\r\t])", ""))
			if(length(DescSIC)==0){ DescSIC<-NA}
			p2<-html_node(pg,css="#resumen_general")
			data<-html_text(html_nodes(p2,css="p")[2])
			Tramo_cap_social<-str_match(data,paste("en el tramo de ","(.+)","\u20AC,",sep=""))[,2]
			Tramo_empleados<-sub(" y ","-",str_match(data,paste("empleados de entre ","(.+)"," y un importe",sep=""))[,2])
			Tramo_ventas<-sub(" y ","-",str_match(data,paste("ventas de entre ","(.+)","\u20AC.",sep=""))[,2])
			ss<-unlist(strsplit(Tramo_ventas,"-"))
			M_c_ventas<-(as.numeric(gsub("[.]","",ss[1]))+as.numeric(gsub("[.]","",ss[2])))/2
			ee<-unlist(strsplit(Tramo_empleados,"-"))
			M_c_empleados<-(as.numeric(gsub("[.]","",ee[1]))+as.numeric(gsub("[.]","",ee[2])))/2
			cc<-sapply(unlist(strsplit(Tramo_cap_social,"-")),str_trim,USE.NAMES = FALSE)
			M_c_cap_social<-(as.numeric(gsub("[.]","",cc[1]))+as.numeric(gsub("[.]","",cc[2])))/2
			geo<-html_nodes(html_nodes(p2,css="div"),css="span")
			lat<-as.numeric(html_text(geo[grep("latitude",capture.output(geo))-1]))
			lng<-as.numeric(html_text(geo[grep("longitude",capture.output(geo))-1]))
			if (sum(lat)==0){lat<-0 ; lng<-0}
			web_aexor<-http
			fila<-(cbind(Prov,Mun,Nombre,Forma_juridica,Tiempo_contituida, Objetivo_social,Direccion,Cod_postal,CodCAE,DescCAE,CodSIC,DescSIC,Tramo_cap_social,Tramo_empleados,Tramo_ventas,M_c_cap_social,M_c_empleados,M_c_ventas,lat,lng,web_aexor))
			return(fila)
}

