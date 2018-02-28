
# Crea las URLs de los Autonomos para extraer informacion


get.empresas.a<-function(http){
			h<-read_html(http)
			l.e<-html_node(h,css="table")
			g<-html_children(html_nodes(html_node(l.e,css="tbody"),css="td"))
			e<-html_attr(g,"href")
			e<-e[complete.cases(e)==TRUE]
			www<-rep("https://autonomos.axesor.es/",length(e))
			w.e<-paste(www,e,sep="")
			w.e
		}