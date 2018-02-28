
# Generate a list muncipalities URLs in Axesor self-employment data base


municipio.a<-function(http){
		base.m<-html_nodes(read_html(http),css="#bloque_listadoMunicipios")
		l.m<-html_attr(html_children(html_nodes(base.m,css="td")),"href")
		mun<-rep(NA,length(l.m))
		for (j in 1:length(l.m)){
			mun[j]<-paste("https://autonomos.axesor.es/",l.m[j],sep="")
		}
		return(mun)
	}