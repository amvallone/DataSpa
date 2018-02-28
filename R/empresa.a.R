
# Extract the self-employmet`s information from the Axesor webpage.
 


empresa.a<-function(http){
			Direccion <- NULL
			Cod_postal <- NULL
			Mun <- NULL
			Prov <- NULL
			CodCAE <- NULL
			DescCAE <- NULL
			CodSIC <- NULL
			DesSIC <- NULL
			pg<-read_html(http)
			Nombre<-str_replace_all(html_text(html_node(pg,css="h3")), "([\n\r\t])", "")
			p1 <- html_node(pg,css=".section-content")
			p3 <- html_nodes(p1,"span")			
			Direccion <- str_trim(str_replace_all(html_text(p3[1]), "([\n\r\t])", ""))
			Cod_postal<-str_trim(str_replace_all(html_text(p3[2]), "([\n\r\t,])", ""))			
			Mun<-str_trim(str_replace_all(html_text(p3[3]), "([\n\r\t,])", ""))
			Prov<-simpleCap(str_trim(str_replace_all(html_text(p3[4]), "([\n\r\t])", "")))
			C.A.E<-html_text(p3[5])
			CodCAE<-na.omit(as.numeric(unlist(strsplit(C.A.E,"[^[:digit:]]"))))[1]
			DescCAE<-str_trim(sub("[[:digit:]]+","",str_replace_all(C.A.E, "([\n\r\t])", "")))
			S.I.C<-html_text(p3[6])
			CodSIC<-na.omit(as.numeric(unlist(strsplit(S.I.C,"[^[:digit:]]"))))[1]
			DescSIC<-str_trim(sub("[[:digit:]]+","",str_replace_all(S.I.C, "([\n\r\t])", "")))
			p2<-html_node(pg,css="#resumen_general")
			data<-html_text(html_nodes(p2,css="p")[2])
			geo<-html_nodes(html_nodes(p2,css="div"),css="span")
			lat<-as.numeric(html_text(geo[grep("latitude",capture.output(geo))-1]))
			lng<-as.numeric(html_text(geo[grep("longitude",capture.output(geo))-1]))
			if (sum(lat)==0){lat<-0 ; lng<-0}
			web_aexor<-http
			fila<-(cbind(Prov,Mun,Nombre ,Direccion,Cod_postal,CodCAE,DescCAE,CodSIC,DescSIC,lat,lng,web_aexor))
			return(fila)
}
