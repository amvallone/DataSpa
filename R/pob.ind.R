#' @name pob.ind
#' @rdname pob.ind
#'
#' @title Calculate the population indexs
#'
#' @description \code{pob.ind} computes a set of demographic indexes
#' @param	year a numerical value from 1996 and the latest available year, which indicates the year of the required database.
#' @param	provincia one of the 52 Spanish provinces.
#' @param print logical variable ‘do you need to print a output file with the results?’ being FALSE the default value.
#'
#' @return A data frame contains the spatial units, in rows, and the demographic index computed in  columns.
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
#'
#' If \code{print} is set to \code{TRUE}, an \code{xlsx} file containing ten sheet -one  data frame per index- is saved into the folder \code{Outputs} which the name  \code{pob_index_provincia_year.xlsx}
#' @references{
#' Junta de Extremadura (2017) Atlas Socieconómico de Extremadura 2017. Mérida (Spain).
#' \url{http://estadistica.gobex.es/web/guest/atlas-socieconomico-de-extremadura}
#'}
#' @family Manipulate functions
#' @examples
#' pob.ind(2012,"Madrid")
#'
#' @export


pob.ind<-function(year,provincia,print=FALSE){
	if(year>2011){cat("Puede obtener un indice mas preciso usando la funcion pob.ind.p() \n")}
	entrada<-pob.q(year,provincia)
	base<-entrada[[1]]
	base.h<-entrada[[2]]
	base.m<-entrada[[3]]
	t<-dim(base)
	infancia<-(apply(base[,4:6],1,sum)/base[,3])*100
	vejez<-(apply(base[,17:t[2]],1,sum)/base[3])*100
	juventud<-(apply(base[,7:9],1,sum)/base[,3])*100
	dependencia<-(apply(base[,c(4:6,17:t[2])],1,sum)/apply(base[,7:16],1,sum))*100
	par<-rep(NA,t[1])
	par.h<-rep(NA,t[1])
	par.m<-rep(NA,t[1])
	if(year>2005){
		d<-paro(year,provincia=provincia)
		v<-intersect(base[,1],d[,1])
		for(j in 1:length(v)){
			b<-which(base[,1]==v[j])
			e<-which(d[,1]==v[j])
			par[b]<-(d[e,3]/apply(base[b,7:16],1,sum))*100
			par.h[b]<-(d[e,4]/apply(base.h[b,7:16],1,sum))*100
			par.m[b]<-(d[e,5]/apply(base.m[b,7:16],1,sum))*100
			}
		}
	edad<-c(seq(2,97,5),101)
	media<-rep(NA,t[1])
	media.h<-rep(NA,t[1])
	media.m<-rep(NA,t[1])
	for(k in 1:t[1]){
		media[k]<-sum(base[k,4:t[2]]*edad)/base[k,3]
		media.h[k]<-sum(base.h[k,4:t[2]]*edad)/base.h[k,3]
		media.m[k]<-sum(base.m[k,4:t[2]]*edad)/base.m[k,3]
	}
	salida<-cbind(base[,1:2],infancia,juventud,vejez,dependencia,par,par.h,par.m,media,media.h,media.m)
	colnames(salida)<-c("Cod","Municipio","Infancia","Juventud","Vejez","Dependencia","Paro Total","Paro Hombres","Paro Mujeres","Edad Media","Edad Media Homres","Edad Media Mujeres")
	num<-sapply(salida,is.numeric)
	salida[num]<-apply(salida[num],2,round,1)
	if(print==TRUE){
			if(dir.exists(file.path(getwd(),"Outputs"))==FALSE){
			dir.create(file.path(getwd(),"Outputs"))
			}
	file<-paste(getwd(),"/Outputs/pob_index_",provincia,"_",year,".xlsx",sep="")
	write.xlsx(base,file)
	}
	salida
}
