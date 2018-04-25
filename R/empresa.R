
# Extract the firm's information from the Axesor webpage.



empresa<-function(http){
			pg<-read_html(http)
			nodes <- html_node(pg,css=".section-content")
			tabla <- html_table(html_children(nodes))[[1]]
			Nombre <- ifelse(length(which(tabla[,1]=="Nombre:"))==0,NA,tabla[which(tabla[,1]=="Nombre:"),2])
			if(is.na(Nombre)){Nombre<-str_replace_all(html_text(html_node(pg,css="h3")), "([\n\r\t])", "")}
			CIF <- ifelse(length(which(tabla[,1]=="CIF:"))==0,NA,tabla[which(tabla[,1]=="CIF:"),2])
			Forma_juridica <- ifelse(length(which(tabla[,1]==paste("Forma jur","\u00ED","dica:",sep="")))==0,NA,tabla[which(tabla[,1]==paste("Forma jur","\u00ED","dica:",sep="")),2])
			Nace <- ifelse(length(which(tabla[,1]=="Constituida hace:"))==0,"N.I",tabla[which(tabla[,1]=="Constituida hace:"),2])
			Objetivo_social <- ifelse(length(which(tabla[,1]=="Objeto social:"))==0,"N.I",str_replace_all(tabla[which(tabla[,1]=="Objeto social:"),2], "([\n\r\t])", ""))
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
			data <- html_text(html_children(p2)[3])
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
			fila<-data.frame("Provincia"=Prov,
							"Municipalidad"=Mun,
							"Nombre"=Nombre,
							"CIF"=CIF,
							"Forma Juridica"=Forma_juridica,
							"Constituida Hace"=Nace,
							"Objeto Social"= Objetivo_social,
							"Direccion"=Direccion,
							"Codigo Postal"=Cod_postal,
							"C.N.A.E"=CodCAE,
							"Descripcion C.N.A.E"=DescCAE,
							"S.I.C"=CodSIC,
							"Descripcion S.I.C"=DescSIC,
							"Tramo Capital Social"=Tramo_cap_social,
							"Tramo Empleados"=Tramo_empleados,
							"Tramo Ventas"=Tramo_ventas,
							"Marca de clase Cap. Social"=M_c_cap_social,
							"Marca de clases empleados"=M_c_empleados,
							"Marca de clase ventas"=M_c_ventas,
							"Latitud"=lat,
							"Longitud"=lng,
							"URL en Axesor"=web_aexor,stringsAsFactors = FALSE)
			return(fila)
}

