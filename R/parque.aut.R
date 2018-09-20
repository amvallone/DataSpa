#'
#' @name parque.aut
#' @rdname parque.aut
#'
#' @title Vehicle fleet information at a municipality level
#'
#' @description \code{parque.aut} collects information about vehicle fleet in each Spanish municipality
#'
#' @param year is numeric variable corresponding to the year, which must be higher than 2013
#'
#' @param ca is a character indicating one of the 17 the Spanish autonomous communities 
#'
#' @param provincia one of the 52 Spanish provincies
#'
#' @return It is a data frame containing the following variables: the municipality name, number and average age of vehicle fleet, type of vehicles (cars, vans, trucks, motorcycles, buses, etc) and some other variables related to the register of drivers, accidents and vehicle taxes. 
#'
#' @details \code{ca} may asume one of these values: "Andalucia", "Asturias", "Aragon", "Baleares", "Canarias","Cantarias", "Castilla y Leon", "Castilla La Mancha", "Cataluña", "Comunidad Valenciana", "Extremadura", "Galicia", "Madrid", "Murcia", "Navarra", "Pais Vasco", "La Rioja", "Ceuta" and  "Melilla".
#' 
#' @examples
#' \dontrun{parque.aut(2014,"Ceuta","Ceuta")}
#'
#' @export



parque.aut<-function(year,ca,provincia){
	year<-as.character(year)
	provincia <- toupper(provincia)
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
	dest<-file.path(getwd(),"DGT",provincia,year)
	myfiles <- NULL
	# ca
	ca<-tolower(ca)
	# Casos raros ca
	if(ca==paste("catalu","\u00F1","a",sep="")){ca<-"catalunia"}
	if(ca=="comunidad valenciana"){ca<-"valencia"}
	if(ca=="pais vasco"){ca<-"paisvasco"}
	if(ca=="la rioja"){ca<-"la-rioja"}
	if(ca=="ceuta"){ca<-"ceuta-melilla"}
	if(ca=="melilla"){ca<-"ceuta-melilla"}
	if(ca=="castilla y leon"){ca<-"castilla-y-leon"}
	if(ca=="castilla la mancha"){ca<-"castilla-la-mancha"}
	
	#provincia
	prov<-tolower(a.letter(provincia))
	# casos raros
	if(prov=="araba"){prov<-"alava"}
	if(prov=="ciudad_real"){prov<-"ciudad-real"}
	if(prov=="a_coruna"){prov<-"corunia"}
	if(prov=="las_palmas"){prov<-"las-palmas"}
	if(prov=="la_rioja"){prov<-"la-rioja"}
	if(prov=="tenerife"){prov <- "santa-cruz-de-tenerife"}

	## carga la lista de txt
	dataset<-list.files(path=dest, pattern="txt", full.names=TRUE)
	if (length(dataset)==0){
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
				tryCatch({
				download.file(mun[j],paste(dest,"/",files[j],sep=""),mode='wb')
				}, error=function(e) return(e))
			}
		}
		Sys.sleep(2)
		myfiles <- list.files(path=dest,pattern = "pdf",  full.names = TRUE)
		lapply(myfiles, function(i) system(paste('pdftotext -eol dos -enc UTF-8 -table', paste0('"', i, '"')), wait = FALSE) )
		Sys.sleep(5)
		} 
		
		dataset<-list.files(path=dest, pattern="txt", full.names=TRUE)
		cod2 <- substr(gsub("[^0-9]","",dataset),9,nchar(dataset))
		print(length(dataset))
		base<-data.frame()
		if(as.numeric(year)<2015){
			for(k in 1:length(dataset)){
				info <- extrae.new(dataset[k])
				base <- rbind(base,info)
			}
		} else {
			for(k in 1:length(dataset)){
				info <- extrae.new(dataset[k])
				base <- rbind(base,info)
			}	
		}
#		if(!is.null(myfiles)) {file.remove(myfiles)}
		base<-as.data.frame(cbind("Cod"=cod2,base))
	return(base)
}



