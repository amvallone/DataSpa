
# Genera una lista numerada de los municipios

nn.municipio<-function(http){
		base.m<-html_nodes(read_html(http),css="#bloque_listadoMunicipios")
		n.m<-html_text(html_children(html_nodes(base.m,css="td")),"href")
		id<-seq(1,length(n.m),1)
		names<-paste("[",id,"] ",n.m,"\n",sep="")
		names
	}