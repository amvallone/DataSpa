#' @name ind.ev
#' @rdname ind.ev
#'
#' @title Panel of demographic indexes at a municipality level for a period of time
#' 
#' @description \code{ind.ev} creates a list containing a panel of demographic indexes of Spain at the municipality level for a period of time from the years \code{inicio} to \code{fin}.
#' @param	inicio starting year of the panel, which must be higher than 1996.
#' @param	fin last year of the panel.
#' @param	provincia one of the 52 Spanish provinces.
#' @param print logical variable ‘do you need to print a output file with the results?’, being FALSE the default value
#'
#' @return It is a list conteining ten data frame.
#'
#' @details This function calculates ten demographical indexes:
#' \itemize{
#'	\item Childhood index
#' 	\item Youth index
#' 	\item Third age index
#' 	\item Dependence index
#' 	\item Unemployment rate, both sexes
#' 	\item Unemployment rate, males
#' 	\item Unemployment rate, females
#' 	\item Municipality average age, both sexes
#' 	\item Municipality average age, males
#'	\item Municipality average age, females
#' }
#' For a full description of the index, see the 2017 Socioeconomic Atlas of Extremadura
#' If \code{print} is set to \code{TRUE}, an \code{xlsx} file containing ten sheet -one  data frame per index- is saved into the folder \code{Outputs} which the name  \code{pob_ev_index_provincia_inicio-fin.xlsx}
#'
#' @references{
#' Junta de Extremadura (2017) Atlas Socieconómico de Extremadura 2017. Mérida (Spain).
#' \url{http://estadistica.gobex.es/web/guest/atlas-socieconomico-de-extremadura}
#'}
#' @family manipulation functions
#' @examples
#' ind.ev(2005,2007,"Avila")
#'
#' @export


ind.ev<-function(inicio,fin,provincia,print=FALSE){
	if(fin<inicio) stop("La fecha de inicio debe ser mayor que la fecha de fin \n")
	n<-seq(inicio,fin,1)
	year<-as.character(sort(n,decreasing=TRUE))
	entrada<-pob.ind(year[1],provincia)
	t<-dim(entrada)[1]
	base.i<-cbind(entrada[,c(1,2,3)])
	base.j<-cbind(entrada[,c(1,2,4)])
	base.v<-cbind(entrada[,c(1,2,5)])
	base.d<-cbind(entrada[,c(1,2,6)])
	base.p<-cbind(entrada[,c(1,2,7)])
	base.ph<-cbind(entrada[,c(1,2,8)])
	base.pm<-cbind(entrada[,c(1,2,9)])
	base.m<-cbind(entrada[,c(1,2,10)])
	base.mh<-cbind(entrada[,c(1,2,11)])
	base.mm<-cbind(entrada[,c(1,2,12)])
		for (i in 2:length(year)){
		aux.i<-rep(NA,t)
		aux.j<-rep(NA,t)
		aux.v<-rep(NA,t)
		aux.d<-rep(NA,t)
		aux.p<-rep(NA,t)
		aux.ph<-rep(NA,t)
		aux.pm<-rep(NA,t)
		aux.m<-rep(NA,t)
		aux.mh<-rep(NA,t)
		aux.mm<-rep(NA,t)
		act<-pob.ind(year[i],provincia)
		v<-intersect(entrada[,1],act[,1])
		for(j in 1:length(v)){
			b<-which(base.i[,1]==v[j])
			e<-which(act[,1]==v[j])
			aux.i[b]<-act[e,3]
			aux.j[b]<-act[e,4]
			aux.v[b]<-act[e,5]
			aux.d[b]<-act[e,6]
			aux.p[b]<-act[e,7]
			aux.ph[b]<-act[e,8]
			aux.pm[b]<-act[e,9]
			aux.m[b]<-act[e,10]
			aux.mh[b]<-act[e,11]
			aux.mm[b]<-act[e,12]
		}
		base.i<-cbind(base.i,aux.i)
		base.j<-cbind(base.j,aux.j)
		base.v<-cbind(base.v,aux.v)
		base.d<-cbind(base.d,aux.d)
		base.p<-cbind(base.p,aux.p)
		base.ph<-cbind(base.ph,aux.ph)
		base.pm<-cbind(base.pm,aux.pm)
		base.m<-cbind(base.m,aux.m)
		base.mh<-cbind(base.mh,aux.mh)
		base.mm<-cbind(base.mm,aux.mm)
		}
	colnames(base.i)<-c("Cod","Municipio",year)
	colnames(base.j)<-c("Cod","Municipio",year)
	colnames(base.v)<-c("Cod","Municipio",year)
	colnames(base.d)<-c("Cod","Municipio",year)
	colnames(base.p)<-c("Cod","Municipio",year)
	colnames(base.ph)<-c("Cod","Municipio",year)
	colnames(base.pm)<-c("Cod","Municipio",year)
	colnames(base.m)<-c("Cod","Municipio",year)
	colnames(base.mh)<-c("Cod","Municipio",year)
	colnames(base.mm)<-c("Cod","Municipio",year)
	orden<-c(1,2,seq(dim(base.i)[2],3))
	base.i<-base.i[,orden]
	base.j<-base.j[,orden]
	base.v<-base.v[,orden]
	base.d<-base.d[,orden]
	base.p<-base.p[,orden]
	base.ph<-base.ph[,orden]
	base.pm<-base.pm[,orden]
	base.m<-base.m[,orden]
	base.mh<-base.mh[,orden]
	base.mm<-base.mm[,orden]
	out<-list(base.i,base.j,base.v,base.d,base.p,base.ph,base.pm,base.m,base.mh,base.mm)
	names(out)<-c("Infancia","Juventud","Vejez","Dependencia","Paro Total","Paro Hombres","Paro Mujeres","Edad Media","Edad Media Hombres","Edad Media Mujeres")
	if(print==TRUE){
		excel<-createWorkbook()
		s1<-addWorksheet(excel,sheetName="Infancia")
		s2<-addWorksheet(excel,sheetName="Juventud")
		s3<-addWorksheet(excel,sheetName="Vejez")
		s4<-addWorksheet(excel,sheetName="Dependencia")
		s5<-addWorksheet(excel,sheetName="Paro Total")
		s6<-addWorksheet(excel,sheetName="Paro Hombres")
		s7<-addWorksheet(excel,sheetName="Paro Mujeres")
		s8<-addWorksheet(excel,sheetName="Edad Media")
		s9<-addWorksheet(excel,sheetName="Edad Media Hombres")
		s10<-addWorksheet(excel,sheetName="Edad Media Mujeres")
		writeDataTable(excel,s1,base.i)
		writeDataTable(excel,s2,base.j)
		writeDataTable(excel,s3,base.v)
		writeDataTable(excel,s4,base.d)
		writeDataTable(excel,s5,base.p)
		writeDataTable(excel,s6,base.ph)
		writeDataTable(excel,s7,base.pm)
		writeDataTable(excel,s8,base.m)
		writeDataTable(excel,s9,base.mh)
		writeDataTable(excel,s10,base.mm)
			if(dir.exists(file.path(getwd(),"Outputs"))==FALSE){
			dir.create(file.path(getwd(),"Outputs"))
			}			
		file<-paste(getwd(),"/Output/pob_ev_index_",provincia,"_",paste(inicio,fin,sep="-"),".xlsx",sep="")
		saveWorkbook(excel,file)
	}
	out
	}
