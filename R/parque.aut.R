#'
#' @name parque.aut
#' @rdname parque.aut
#'
#' @title Municipality vehicle fleet information
#'
#' @description Collect the municipality vehicle fleet information
#'
#' @param year is numeric variable expressing the year of interest, must be higher than 2013#' @param ca is a character indicating one of the 17 the autonomous community#' @param provincia one of the 52 Spanish provincies
#'
#' @return A data frame
#'
#' @details \code{ca} may asume one of these values: "Andalucia", "Asturias", "Aragon", "Baleares", "Canarias", "Castilla y Leon", "Castilla-La Mancha", "Catalu\u00F1a", "Comunidad Valenciana", "Extremadura", "Galicia", "Madrid", "Murcia", "Navarra", "Pais Vasco", "La Rioja" and "Ceuta y Melilla".
#' 
#' For more detail information about Spanish Autonomus Comunities and provincies visit \href{https://es.wikipedia.org/wiki/Anexo:Provincias_y_ciudades_aut%C3%B3nomas_de_Espa%C3%B1a}{Wikipedia Anexo}
#' @examples
#' \dontrun{parque.aut(2014,"Ceuta y Melilla","Ceuta")}
#'
#' @export

##Cataluña Catalunia
##Comunidad Valenciana Valencia
## Pais Vasco paisvasco
## La Rioja la-rioja
## Ceuta y Melilla  ceuta-melilla

parque.aut<-function(year,ca,provincia){
	year<-as.character(year)
	ca<-tolower(ca)
	if(ca=="catalu\u00F1a"){ca<-"catalunia"}
	if(ca=="comunidad valenciana"){ca<-"valencia"}
	if(ca=="pais vasco"){ca<-"paisvasco"}
	if(ca=="la rioja"){ca<-"la-rioja"}
	if(ca=="ceuta y melilla"){ca<-"ceuta-melilla"}
	if(ca=="castilla y leon"){ca<-"castilla-y-leon"}
	if(ca=="castilla-la mancha"){ca<-"castilla-la-mancha"}
	prov<-a.letter(tolower(provincia))
	##Crea los directorios
	if (dir.exists(file.path(getwd(),"DGT"))==FALSE){
		dir.create(file.path(getwd(),"DGT"))	
		}
	if (dir.exists(file.path(getwd(),"DGT",provincia))==FALSE){	
		dir.create(file.path(getwd(),"DGT",provincia))
		}
	if (dir.exists(file.path(getwd(),"DGT",provincia,year))==FALSE){
		dir.create(file.path(getwd(),"DGT",provincia,year))
		}
	## carga la lista de txt
	dest<-file.path(getwd(),"DGT",provincia,year)
	dataset<-list.files(path=dest, pattern="txt", full.names=TRUE)
	if (length(dataset)==0) {
	#descarga los archivos
	page<-paste("http://www.dgt.es/es/seguridad-vial/estadisticas-e-indicadores/informacion-municipal/provincias/",year,"/",ca,"/",prov,".shtml",sep="")
	p<-read_html(page)
	n<-xml_nodes(p,css=".tabg")
	mun<-html_attr(html_nodes(n,css="a"),"href")
	nn.mun<-str_trim(html_text(html_nodes(n,css="a")))
	aux<-sapply(mun,strsplit,"/",USE.NAMES=FALSE)
	len<-unlist(lapply(aux,length))
	files<-rep("",length(len))
	for ( i in seq_along(len)){
		files[i]<-aux[[i]][len[i]]
	}
	cod <- gsub("[^0-9]","",files)
	pat<-c(", "," ","\u00E9","\u00E1","\u00ED","\u00F3","\u00FA","\u00C1","\u00C9","\u00CD","\u00D3","\u00DA","\u00F1","\u00D1")
	rempl<-c("_","_","e","a","i","o","u","A","E","I","O","U","n","N")
	for (i in 1:length(pat)){
		nn.mun<-sapply(nn.mun,str_replace_all,pat[i],rempl[i],USE.NAMES=FALSE)
		mun<-sapply(mun,str_replace_all,pat[i],rempl[i],USE.NAMES=FALSE)
		}
	files<-paste(year,"_",cod,"_",nn.mun,".pdf",sep="")
	for (j in 1:length(files)){
		if(sum(dir(dest)==files[j])==0){
			download.file(mun[j],paste(dest,"/",files[j],sep=""),mode='wb')
		}
	}
	myfiles <- list.files(path=dest,pattern = "pdf",  full.names = TRUE)
	lapply(myfiles, function(i) system(paste('pdftotext -eol dos -enc UTF-8 -table', paste0('"', i, '"')), wait = FALSE) )
	file.remove(myfiles)
	} 
		dataset<-list.files(path=dest, pattern="txt", full.names=TRUE)
		cod2 <- substr(gsub("[^0-9]","",dataset),9,nchar(dataset))
		print(length(dataset))
		base<-data.frame()
		if(as.numeric(year)<2015){
			for(k in 1:length(dataset)){
				data<-read.table(dataset[k],sep="\t",skip=1)
				a<-capture.output(data)	
				Seg<-matrix("A",8,2)
					for (i in 4:11){
						aux<-str_trim(substring(a[i],120,nchar(a[i])))
						aux<-as.matrix(unlist(strsplit(aux," ")))
						aux<-as.vector(aux[which(nchar(aux)!=0),])
						Seg[i-3,1]<-paste0(aux[1:(length(aux)-1)],collapse=" ")
						Seg[i-3,2]<-aux[length(aux)]
					}

				Censo.Cond<-matrix("A",3,2)
					for (i in 8:10){
						aux<-str_trim(substring(a[i],1,100))
						aux<-as.matrix(unlist(strsplit(aux," ")))
						aux<-as.vector(aux[which(nchar(aux)!=0),])
						if (i==8){
							Censo.Cond[i-7,1]<-paste0(aux[2:(length(aux)-1)],collapse=" ")
						} else {
							Censo.Cond[i-7,1]<-paste0(c("Conductores",aux[2]),collapse=" ")
						}
						Censo.Cond[i-7,2]<-aux[length(aux)]
					}

				sinITV<-matrix("A",3,2)
					for(i in 40:42){
						aux<-as.matrix(unlist(strsplit(a[i]," ")))
						aux<-as.vector(aux[which(nchar(aux)!=0),])
						sinITV[i-39,1]<-paste(str_trim(aux[2]),"sin ITV en vigor",collapse=" ")
						sinITV[i-39,2]<-str_trim(aux[3])
					}

				parque<-matrix("A",6,2)
				antiguedad<-matrix("A",6,2)
					for(i in 16:21){
						aux<-as.matrix(unlist(strsplit(a[i]," ")))
						aux<-as.vector(aux[which(nchar(aux)!=0),])
						if(i==18) {aux<-aux[1:5]}
						if(i==16) {aux<-c(aux[1],paste0(aux[2:4],collapse=" "),aux[5],"100",aux[6])}
						parque[i-15,1]<-paste0(c("Parque",aux[2]),collapse=" ")
						antiguedad[i-15,1]<-paste0(c("Antiguedad",aux[2]),collapse=" ")
						parque[i-15,2]<-str_trim(aux[3])
						antiguedad[i-15,2]<-str_trim(aux[5])
					}
				nn<-as.matrix(unlist(strsplit(a[2]," ")))
				nn<-as.vector(nn[which(nchar(nn)!=0),])
				nn<-nn[which(nn=="Municipio:"):(which(nn=="Provincia:")-1)]
				if(length(nn)>2){
					Nombre<-matrix(c(nn[1],paste0(nn[2:length(nn)],collapse=" ")),1,2)
					} else {
					Nombre<-matrix(c(nn[1:2]),1,2)
					}
				salida<-t(rbind(Nombre, parque, antiguedad, Censo.Cond, Seg))
				cols<-salida[1,]
				colnames(salida)<-cols
				salida<-salida[-1,]
				salida<-as.data.frame(t(salida)) ; rownames(salida)<-NULL	
				base<-rbind(base,salida)
			}
		} else {
			for(k in 1:length(dataset)){
				data<-read.table(dataset[k],sep="\t",skip=1)
				a<-capture.output(data)	
				Seg<-matrix("A",5,2)
				for (i in 4:8){
					aux1<-str_trim(substring(a[i],120,nchar(a[i])))
					aux<-as.matrix(unlist(strsplit(aux1," ")))
					aux<-as.vector(aux[which(nchar(aux)!=0),])
					Seg[i-3,1]<-paste0(aux[1:(length(aux)-1)],collapse=" ")
					Seg[i-3,2]<-aux[length(aux)]
					if(i==7){Seg[i-3,1]<-substring(aux1,str_locate(aux1,"Sanciones")[1],str_locate(aux1,"2015")[2])}
					if(i==8){Seg[i-3,1]<-substring(aux1,str_locate(aux1,"Puntos")[1],str_locate(aux1,"2015")[2])}
		 			if(Seg[i-3,2]=="2015" && nchar(aux1)<99) Seg[i-3,2]<-"NA"
					}

			Censo.Cond<-matrix("A",3,2)
				for (i in 8:10){
					aux<-str_trim(substring(a[i],1,110))
					if(i==9) {aux<-a[i]}
					aux<-as.matrix(unlist(strsplit(aux," ")))
					aux<-as.vector(aux[which(nchar(aux)!=0),])
					Censo.Cond[i-7,1]<-paste0(c("Conductores",aux[2]),collapse=" ")
					Censo.Cond[i-7,2]<-aux[length(aux)]
				}

			sinITV<-matrix("A",3,2)
				for(i in 38:40){
					aux<-as.matrix(unlist(strsplit(a[i]," ")))
					aux<-as.vector(aux[which(nchar(aux)!=0),])
					sinITV[i-37,1]<-paste(str_trim(aux[2]),"sin ITV en vigor",collapse=" ")
					sinITV[i-37,2]<-str_trim(aux[3])
			}

			parque<-matrix("A",6,2)
			antiguedad<-matrix("A",6,2)
			for(i in 15:20){
				aux<-as.matrix(unlist(strsplit(a[i]," ")))
				aux<-as.vector(aux[which(nchar(aux)!=0),])
				if(i==18) {aux<-aux[1:5]}
				if(i==15) {aux<-c(aux[1],paste0(aux[2:5],collapse=" "),aux[6],"100",aux[7])}
				parque[i-14,1]<-paste0(c("Parque",aux[2]),collapse=" ")
				antiguedad[i-14,1]<-paste0(c("Antiguedad",aux[2]),collapse=" ")
				parque[i-14,2]<-str_trim(aux[3])
				antiguedad[i-14,2]<-str_trim(aux[5])
			}
			nn<-as.matrix(unlist(strsplit(a[2]," ")))
			nn<-as.vector(nn[which(nchar(nn)!=0),])
			nn<-nn[which(nn=="Municipio:"):(which(nn=="Provincia:")-1)]
			if(length(nn)>2){
				Nombre<-matrix(c(nn[1],paste0(nn[2:length(nn)],collapse=" ")),1,2)
			} else {
				Nombre<-matrix(c(nn[1:2]),1,2)
			}
			salida<-t(rbind(Nombre, parque, antiguedad, Censo.Cond, Seg))
			cols<-salida[1,]
			colnames(salida)<-cols
			salida<-salida[-1,]
			salida<-as.data.frame(t(salida)) ; rownames(salida)<-NULL	
			base<-rbind(base,salida)
			}	
		}
		base<-as.data.frame(cbind("Cod"=cod2,base))
	return(base)
}



