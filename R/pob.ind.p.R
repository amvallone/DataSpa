#' @name pob.ind.p
#' @rdname pob.ind.p
#'
#' @title Calculate the population indexs
#'
#' @description Compute a set of population index
#' @param	year A numerical value between 1996 and the current year indicating the year of the required data.
#' @param	provincia one of the 52 Spanish provinces.
#' @param print Logical variable, do you need print a output file with the results?. Default value is FALSE
#'
#' @return a data frame
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
#' The principal difference between this function and \code{pob.ind()} is the precision of the calculaton. For more details look for El atlas....
#'
#' @examples
#' pob.ind.p(2012,"Madrid")
#'
#' @export



pob.ind.p<-function(year,provincia,print=FALSE){
	if(year<2011) stop("No existe datos para estos casos")
	input<-pob.a(year,provincia)
	base<-input[[1]]
	base.h<-input[[2]]
	base.m<-input[[3]]
	t<-dim(base)
	infancia<-(apply(base[,4:18],1,sum)/base[,3])*100
	vejez<-(apply(base[,69:dim(base)[2]],1,sum)/base[3])*100
	juventud<-(apply(base[,19:33],1,sum)/base[,3])*100
	dependencia<-(apply(base[,c(4:19,69:t[2])],1,sum)/apply(base[,20:68],1,sum))*100
	edad<-c(0.5,seq(1,101,1))
	par<-rep(NA,t[1])
	par.h<-rep(NA,t[1])
	par.m<-rep(NA,t[1])
	if(year>2005){
		d<-paro(year,provincia=provincia)
		v<-intersect(base[,1],d[,1])
		for(j in 1:length(v)){
			b<-which(base[,1]==v[j])
			e<-which(d[,1]==v[j])
			par[b]<-(d[e,3]/apply(base[b,20:68],1,sum))*100
			par.h[b]<-(d[e,4]/apply(base.h[b,20:68],1,sum))*100
			par.m[b]<-(d[e,5]/apply(base.m[b,20:68],1,sum))*100
			}
		}
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
	file<-paste(getwd(),"/Outputs/pob_index-p_",provincia,"_",year,".xlsx",sep="")
	write.xlsx(base,file)
		}
	salida
}
