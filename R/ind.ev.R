#' @name ind.ev
#' @rdname ind.ev
#'
#' @title Population index evolution
#' 
#' @description Create a list containing the population index evolution at municipality level between the \code{inicio} and \code{fin} years.
#' @param	inicio starting year of the panel. Must be higher than 1996.
#' @param	fin last year of the panel.
#' @param	provincia one of the 52 Spanish provinces.
#' @param print Logical variable, do you need print a output file with the results?. Default value is FALSE
#'
#' @return a list conteining ten data frame.
#'
#' @details in this function 10 index are comptuted:
#' \itemize{
#'	\item Childhood index
#' 	\item Youthfulness index
#' 	\item Eld index
#' 	\item Dependence index
#' 	\item Total unemployment rate
#' 	\item Male unemployment rate
#' 	\item Femal unemployment rate
#' 	\item Total Population average age
#' 	\item Male population average age
#'	\item Female population average age
#' }
#' 
#' If \code{print} is \code{TRUE}, a \code{xlsx} file containing ten sheet, one  data frame per index, is saved into the folder \code{Outputs} by the name:
#' \code{pob_ev_index_provincia_inicio-fin.xlsx}
#'
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
		s1<-createSheet(excel,sheetName="Infancia")
		s2<-createSheet(excel,sheetName="Juventud")
		s3<-createSheet(excel,sheetName="Vejez")
		s4<-createSheet(excel,sheetName="Dependencia")
		s5<-createSheet(excel,sheetName="Paro Total")
		s6<-createSheet(excel,sheetName="Paro Hombres")
		s7<-createSheet(excel,sheetName="Paro Mujeres")
		s8<-createSheet(excel,sheetName="Edad Media")
		s9<-createSheet(excel,sheetName="Edad Media Hombres")
		s10<-createSheet(excel,sheetName="Edad Media Mujeres")
		addDataFrame(base.i,s1)
		addDataFrame(base.j,s2)
		addDataFrame(base.v,s3)
		addDataFrame(base.d,s4)
		addDataFrame(base.p,s5)
		addDataFrame(base.ph,s6)
		addDataFrame(base.pm,s7)
		addDataFrame(base.m,s8)
		addDataFrame(base.mh,s9)
		addDataFrame(base.mm,s10)
			if(dir.exists(file.path(getwd(),"Outputs"))==FALSE){
			dir.create(file.path(getwd(),"Outputs"))
			}			
		file<-paste(getwd(),"/Output/pob_ev_index_",provincia,"_",paste(inicio,fin,sep="-"),".xlsx",sep="")
		saveWorkbook(excel,file)
	}
	out
	}
