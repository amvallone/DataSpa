
# Generate a list containing the Firm’s URLs in Axesor web site



lista.empresa<-function(http){
	h<-read_html(http)
	doc<-htmlParse(h)
	txt<-capture.output(doc)
	v<-grep("paginacion-botones",txt)
		if (sum(v)==0){
			salida<-get.empresas(http)
		} else {
			n.p<-html_attr(html_children(html_node(h,css=".paginacion-numeracion")),"href")
			pag<-as.numeric(str_extract(n.p[length(n.p)],"[[:digit:]]+"))
			times<-as.character(seq(1,pag))
			p.g1<-rep(substr(http,1,nchar(http)-1),length(times))
			paginas<-paste(p.g1,times,sep="")
			salida<-lapply(paginas,get.empresas)
			salida<-unlist(salida,use.names=FALSE)
			}
		salida
}