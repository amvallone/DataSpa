#' @name pob.fen.ev
#' @rdname pob.fen.ev
#'
#' @title Panel of the number of births and deaths at a municipality level for a period of time
#' 
#' @description \code{pob.fen.ev} creates a list with the panel of the number of births and deaths of Spain at the municipality level for a period of time from the years  \code{inicio} to \code{fin}.
#' @param	inicio starting year of the panel, which must be higher than 1996..
#' @param	fin last year of the panel.
#' @param	provincia one of the 52 Spanish provinces.
#' @param print logical variable ‘do you need print a output file with the results?’ for which FALSE is the default value.
#'
#' @return It is list containing two data frames for the number of births  (Nacimientos) and deaths (Falleciemientos) at the municpality level.
#'
#' @details If \code{print} is set to \code{TRUE}, an \code{xlsx} file containing two sheet, one per each variable data frame, is saved into the folder \code{Outputs} called \code{fen_evol_total_provincia_inicio-fin.xlsx}
#'
#' @family Manipulate functions
#' @examples
#' pob.fen.ev(2005,2007,"Avila")
#'
#' @export


pob.fen.ev<-function(inicio,fin,provincia,print=FALSE){
	if(fin<inicio) stop("La fecha de inicio debe ser mayor que la fecha de fin")
	n<-seq(inicio,fin,1)
	year<-as.character(sort(n,decreasing=TRUE))
	base<-pob.fen(year[1],provincia)
	base.n<-cbind(base[,c(1,2,3)])
	base.f<-cbind(base[,c(1,2,4)])
	for (i in 2:length(year)){
		aux1<-rep(NA,dim(base)[1])
		aux2<-rep(NA,dim(base)[1])
		pob<-pob.fen(year[i],provincia)
		v<-intersect(base[,1],pob[,1])
		for(j in 1:length(v)){
			aux1[which(base[,1]==v[j])]<-pob[which(pob[,1]==v[j]),3]
			aux2[which(base[,1]==v[j])]<-pob[which(pob[,1]==v[j]),4]
		}
		base.n<-cbind(base.n,aux1)
		base.f<-cbind(base.f,aux2)
		}
	colnames(base.n)<-c("Cod","Municipio",year)
	colnames(base.f)<-c("Cod","Municipio",year)
	orden<-c(1,2,seq(dim(base.n)[2],3))
	base.n<-base.n[,orden]
	base.f<-base.f[,orden]
	if(print==TRUE){
			if(dir.exists(file.path(getwd(),"Outputs"))==FALSE){
			dir.create(file.path(getwd(),"Outputs"))
			}		
		excel<-createWorkbook()
		s1<-addWorksheet(excel,sheetName="Nacimientos")
		s2<-addWorksheet(excel,sheetName="Fallacimientos")
		writeDataTable(excel,s1,base.n)
		writeDataTable(excel,s2,base.f)
		file<-paste(getwd(),"/Outputs/fen_evol_total_",provincia,"_",paste(inicio,fin,sep="-"),".xlsx",sep="")
		saveWorkbook(excel,file)
	}
	base<-list(base.n,base.f)
	names(base)<-c("Nacimientos","Fallecimientos")
	base
}
