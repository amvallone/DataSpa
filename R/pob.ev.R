#' @name pob.ev
#' @rdname pob.ev
#'
#' @title Panel of total population at the municipality level for a period of time
#' 
#' @description Create a data frame containing a panel of total population at the municipality level from the years \code{inicio} to \code{fin}.
#' @param	inicio starting year of the panel, which must be higher than 1996.#' @param	fin last year of the panel.#' @param	provincia one of the 52 Spanish provinces.
#' @param print logical variable ‘do you need to print a output file with the results?’ being FALSE is the default value.
#'
#' @return It is a data frame
#'
#' @details If \code{print} is set to  \code{TRUE}, an \code{xlsx} file containing the data frame is saved in the folder \code{Outputs} called \code{pob_total_ev_provincia_inicio-fin.xlsx}
#'
#' @family Manipulate functions
#' @examples
#' pob.ev(2005,2007,"Avila")
#'
#' @export

pob.ev<-function(inicio,fin,provincia,print=FALSE){
	if(fin<inicio) stop("La fecha de inicio debe ser mayor que la fecha de fin")
	n<-seq(inicio,fin,1)
	year<-as.character(sort(n,decreasing=TRUE))
	base<-pob.tot(year[1],provincia)
	for (i in 2:length(year)){
		aux<-rep(NA,dim(base)[1])
		pob<-pob.tot(year[i],provincia)
		v<-intersect(base[,1],pob[,1])
		for(j in 1:length(v)){
			aux[which(base[,1]==v[j])]<-pob[which(pob[,1]==v[j]),3]
		}
		base<-cbind(base,aux)
		}
	colnames(base)<-c("Cod","Municipio",year)
	orden<-c(1,2,seq(dim(base)[2],3))
	base<-base[,orden]
		if (print==TRUE){
			if(dir.exists(file.path(getwd(),"Outputs"))==FALSE){
			dir.create(file.path(getwd(),"Outputs"))
			}
		file<-paste(getwd(),"/Outputs/pob_total_ev_",provincia,"_",paste(inicio,fin,sep="-"),".xlsx",sep="")
		openxlsx::write.xlsx(base,file)		
		}
	base
}
