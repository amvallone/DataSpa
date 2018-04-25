
# Extract the self-employmet`s information from the Axesor webpage.
 


empresa.a<-function(http){
			pg<-read_html(http)
			nodes <- html_node(pg,css=".section-content")
			tabla <- html_table(html_children(nodes))[[1]]
			Nombre <- ifelse(length(which(tabla[,1]==" Autónomo / Profesional:"))==0,NA,tabla[which(tabla[,1]==" Autónomo / Profesional:"),2])
			if(is.na(Nombre)){Nombre<-str_replace_all(html_text(html_node(pg,css="h3")), "([\n\r\t])", "")}
			C.A.E <- ifelse(length(which(tabla[,1]=="CNAE:"))==0,NA,tabla[which(tabla[,1]=="CNAE:"),2])
			if(!is.na(C.A.E)){
				CodCAE<-na.omit(unlist(strsplit(C.A.E,"[^[:digit:]]")))[1]
				DescCAE<-sub("[[:digit:]]+","",str_replace_all(C.A.E, "([\n\r\t])", ""))
				if(length(DescCAE)==0){ DescCAE<-NA}
			} else {
				CodCAE<-NA
				DescCAE<-NA
			}
			S.I.C <- ifelse(length(which(tabla[,1]=="SIC:"))==0,NA,tabla[which(tabla[,1]=="SIC:"),2])
			if(!is.na(S.I.C)){
				CodSIC<-na.omit(unlist(strsplit(S.I.C,"[^[:digit:]]")))[1]
				DescSIC<-sub("[[:digit:]]+","",str_replace_all(S.I.C, "([\n\r\t])", ""))
				if(length(DescSIC)==0){ DescSIC<-NA}
			} else {
				CodSIC<-NA
				DescSIC<-NA
			}
			Dir <- ifelse(length(which(tabla[,1]==paste("Direcci","\u00F3","n:",sep="")))==0,NA,tabla[which(tabla[,1]==paste("Direcci","\u00F3","n:",sep="")),2])
			if(!is.na(Dir)){
				Dd <- html_children(html_node(nodes,"span"))
				if(length(Dd)==0){Dd <- html_nodes(nodes,"span")}
				Direccion <- str_trim(str_replace_all(html_text(Dd[1]), "([\n\r\t,])", ""))
				Cod_postal <- str_trim(str_replace_all(html_text(Dd)[2], "([\n\r\t,])", ""))
				Mun <- simpleCap(str_trim(str_replace_all(html_text(Dd)[3], "([\n\r\t,])", "")))
				Prov <- simpleCap(str_trim(str_replace_all(html_text(Dd)[4], "([\n\r\t,])", "")))
			} else {
				Direccion <- NA
				Cod_postal <- NA
				Mun <- NA
				Prov <- NA
			}		
			p2<-html_node(pg,css="#resumen_general")
			data<-html_text(html_nodes(p2,css="p")[2])
			geo<-html_nodes(html_nodes(p2,css="div"),css="span")
			lat<-as.numeric(html_text(geo[grep("latitude",capture.output(geo))-1]))
			lng<-as.numeric(html_text(geo[grep("longitude",capture.output(geo))-1]))
			if (sum(lat)==0){lat<-0 ; lng<-0}
			web_aexor<-http
						fila<-data.frame("Provinciaa"=Prov,
							"Municipalidad"=Mun,
							"Nombre"=Nombre,
							"Direccion"=Direccion,
							"Codigo Postal"=Cod_postal,
							"C.N.A.E"=CodCAE,
							"Descripción C.N.A.E"=DescCAE,
							"S.I.C"=CodSIC,
							"Descripcion S.I.C"=DescSIC,
							"Latitud"=lat,
							"Longitud"=lng,
							"URL en Axesor"=web_aexor,stringsAsFactors = FALSE)
			return(fila)
}
