#' @name pob.ev
#' @rdname pob.ev
#'
#' @title Total population evolution
#' 
#' @description Create a data drame containing the provincie total population evolution at municipality level between the \code{inicio} and \code{fin} years.
#' @param	inicio starting year of the panel. Must be higher than 1996.
#' @param print Logical variable, do you need print a output file with the results?. Default value is FALSE
#'
#' @return a data frame
#'
#' @details If \code{print} is \code{TRUE}, a \code{xlsx} file containing the data frame is saved into the folder \code{Outputs} by the name:
#'
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
		xlsx::write.xlsx(base,file)		
		}
	base
}