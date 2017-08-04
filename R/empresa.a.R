#' @name empresa.a
#' @rdname empresa.a
#'
#' @title Extract self-emplyment information
#'
#' @description Extract the self-employmet`s information from the Axesor webpage.
#'
#' @param http caracther with the Axesor self-employment URL
#'
#' @return A data frame
#'
#' @details for variable contained in the data frame see XX 
#'
#' @examples
#' \dontrun{empresa.a("https://autonomos.axesor.es//informe-de-autonomo/autonomo/Moussa-Gassa/5912075")}
#'
#' @export 


empresa.a<-function(http){
			pg<-read_html(http)
			Nombre<-str_replace_all(html_text(html_node(pg,css="h3")), "([\n\r\t])", "")
			p1<-html_children(html_nodes(html_node(pg,css=".section-content"),css="dl"))
			Tiempo_contituida<-sub("Constituida hace.","",str_replace_all(html_text(p1[grep("Constituida hace",capture.output(p1))],trim=TRUE), "([\n\r\t])", ""))
			if(length(Tiempo_contituida)==0) {Tiempo_contituida<-"N.I"}
			Dir<-html_text(html_children(html_children(p1[grep("Direcci\u00F3n de",capture.output(p1))-1])))
			if (length(Dir)==0) {Dir<-html_text(html_nodes(p1[grep("Direcci\u00F3n",capture.output(p1))],css="span"))}
			Direccion<-str_trim(str_replace_all(Dir[1], "([\n\r\t,])", ""))
			Cod_postal<-str_trim(str_replace_all(Dir[2], "([\n\r\t,])", ""))
			Mun<-simpleCap(str_trim(str_replace_all(Dir[3], "([\n\r\t,])", "")))
			Prov<-simpleCap(str_trim(str_replace_all(Dir[4], "([\n\r\t])", "")))
			C.A.E<-html_text(p1[grep("CNAE",capture.output(p1))])
			CodCAE<-na.omit(as.numeric(unlist(strsplit(C.A.E,"[^[:digit:]]"))))[1]
			DescCAE<-str_trim(sub("[[:digit:]]+","",str_replace_all(C.A.E, "([\n\r\t])", "")))
			if(length(DescCAE)==0){ DescCAE<-NA}
			S.I.C<-html_text(p1[grep("SIC",capture.output(p1))])
			CodSIC<-na.omit(as.numeric(unlist(strsplit(S.I.C,"[^[:digit:]]"))))[1]
			DescSIC<-str_trim(sub("[[:digit:]]+","",str_replace_all(S.I.C, "([\n\r\t])", "")))
			if(length(DescSIC)==0){ DescSIC<-NA}
			p2<-html_node(pg,css="#resumen_general")
			data<-html_text(html_nodes(p2,css="p")[2])
			geo<-html_nodes(html_nodes(p2,css="div"),css="span")
			lat<-as.numeric(html_text(geo[grep("latitude",capture.output(geo))-1]))
			lng<-as.numeric(html_text(geo[grep("longitude",capture.output(geo))-1]))
			if (sum(lat)==0){lat<-0 ; lng<-0}
			web_aexor<-http
			fila<-(cbind(Prov,Mun,Nombre,Tiempo_contituida ,Direccion,Cod_postal,CodCAE,DescCAE,CodSIC,DescSIC,lat,lng,web_aexor))
			return(fila)
}
