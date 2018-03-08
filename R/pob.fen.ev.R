#' @name pob.fen.ev
#' @rdname pob.fen.ev
#'
#' @title Population phenomena evolution
#' 
#' @description Create a list containing the provincie population phenomena evolution at municipality level between the \code{inicio} and \code{fin} years.
#' @param	inicio starting year of the panel. Must be higher than 1996.
#' @param	fin last year of the panel.
#' @param	provincia one of the 52 Spanish provinces.
#' @param print Logical variable, do you need print a output file with the results?. Default value is FALSE
#'
#' @return a list conteining two data frame one of birth population at municpality level (Nacimientos) and other with death population at muncipality level (Falleciemientos)
#'
#' @details If \code{print} is \code{TRUE}, a \code{xlsx} file containing two sheet, one per each data frame, is saved into the folder \code{Outputs} by the name:
#' \code{fen_evol_total_provincia_inicio-fin.xlsx}
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
		s1<-createSheet(excel,sheetName="Nacimientos")
		s2<-createSheet(excel,sheetName="Fallacimientos")
		addDataFrame(base.n,s1)
		addDataFrame(base.f,s2)
		file<-paste(getwd(),"/Outputs/fen_evol_total_",provincia,"_",paste(inicio,fin,sep="-"),".xlsx",sep="")
		saveWorkbook(excel,file)
	}
	base<-list(base.n,base.f)
	names(base)<-c("Nacimientos","Fallecimientos")
	base
}
