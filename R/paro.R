
#' @name paro
#' @rdname paro
#'
#' @title import information of municipality unemploymet into R.
#'
#' @description Import the work force data of the province`s municipalities corresponding to the required. 
#'
#' @param year A numerical value between 2005 and the current year indicating the year of the required data.
#' @param mes one of the 12 moths in Spanish indicating the moth when the data was collected.
#' @param provincia one of the 52 Spain’s province.
#'
#' @return data frame containing the fallowing variables.
#' - \code{cod} is the municipality identification number based in the INE codification.#' - \code{Name} the municipality name.#' - \code{Total unemployment} number of unemployed people in the municipality #' - \code{Total male unemployment} number of unemployes males in the municipality#' - \code{Total female unemployment} number of unemployees females in the municipality
#'
#' @examples
#' #paro(2005,"julio","Madrid")
#'
#' @export



paro<-function(year,mes="julio",provincia){
	year<-as.character(year)
	Cap<-provincia
	provincia<-toupper(provincia)
	provincia<-a.letter(provincia)
	mes<-tolower(mes)
	nn.mes<-seq(1,12,1)
	names(nn.mes)<-c("enero","febrero","marzo","abril","mayo","junio","julio","agosto","septiembre","octubre","noviembre","diciembre")
	cod<-paste("0",nn.mes[mes],substr(year,3,4),sep="")
	dirc<-paste(getwd(),"/data_paro/",sep="")
	name<-paste(paste("MUNI",provincia,cod,sep="_"),".xls",sep="")
	file<-paste("paro_",name,sep="")
	if(sum(dir(dirc)==file)==0){
		getbase.paro(year,mes,provincia)
	}
	if(as.numeric(year)<2013){
		cod1<-paste("0",nn.mes[mes],"16",sep="")
		name1<-paste(paste("MUNI",provincia,cod1,sep="_"),".xls",sep="")
		file1<-paste("paro_",name1,sep="")
		if(sum(dir(dirc)==file1)==0){
			getbase.paro(2016,mes,provincia)
		}
		open<-paste(dirc,file1,sep="")
		abre<-paste(dirc,file,sep="")
		datos<-xlsx::read.xlsx(abre,1)
		idn<-xlsx::read.xlsx(open,1,colIndex=c(1:3))
		datos<-apply(datos,2,as.character)
		idn<-apply(idn,2,as.character)
		datos<-datos[-dim(datos)[1],]
		idn<-idn[-dim(idn)[1],]
		p<-which(datos[,1]=="MUNICIPIOS")+1
		f<-dim(datos)[1]
		pi<-max(which(is.na(idn[,1])))+1
		fi<-dim(idn)[1]
		datos<-datos[p:f,]
		datos<-cbind(datos[,1],datos)
		v<-intersect(datos[,1],idn[,2])
		for( k in 1:length(v)){
			if(length(which(idn[,2]==v[k]))>1){
				datos[which(datos[,2]==v[k]),1]<-idn[which(idn[,2]==v[k])[2],1]
			}else{
				datos[which(datos[,2]==v[k]),1]<-idn[which(idn[,2]==v[k]),1]
			}
		}
		if(stri_count(datos[8,1],regex="[[:number:]]")==4){
			cero<-rep(0,dim(datos)[1])
			datos[,1]<-paste(cero,datos[,1],sep="")
		}
		datos<-datos[-1,]
		cod<-datos[,1]
		n.m<-datos[,2]
		total<-as.numeric.factor(datos[,3])
		total.h<-NA
		total.m<-NA
		salida<-as.data.frame(cbind(cod,n.m,total,total.h,total.m))
		salida[,3:5]<-apply(salida[,3:5],2,as.numeric)
		salida[,1]<-as.character(salida[,1])
		salida[,2]<-as.character(salida[,2])
		colnames(salida)<-c("cod","nombre","paro total","paro total hombres","paro total mujeres")
		salida[which(is.na(salida[,3])),3]<-0
		salida[which(is.na(salida[,4])),4]<-0
		salida[which(is.na(salida[,5])),5]<-0
		salida<-rbind(salida,rep(NA,5))
		nd<-dim(salida)[1]
		salida[nd,1]<-"Total"
		salida[nd,2]<-Cap
		salida[nd,3]<-sum(salida[1:nd-1,3])
		salida[nd,4]<-sum(salida[1:nd-1,4])
		salida[nd,5]<-sum(salida[1:nd-1,5])
		fallas<-apply(as.matrix(salida[,1]),1,nchar)
		n.fallas<-which(fallas!=5)
		if (length(n.fallas)!=0){
			for (i in seq_along(n.fallas)){
				salida[n.fallas[i],1]<-codifica(salida[n.fallas[i],2],provincia)
			}
		}
		salida
	} else {
		abre<-paste(dirc,file,sep="")
		datos<-xlsx::read.xlsx(abre,1)
		datos<-apply(datos,2,as.character)
		datos<-datos[-dim(datos)[1],]
		p<-max(which(is.na(datos[,1])))+1
		f<-dim(datos)[1]
		ind<-datos[p:f,1]
		if(stri_count(ind[8],regex="[[:number:]]")==4){
			cero<-rep(0,length(ind))
			ind<-paste(cero,ind,sep="")
		}
		n.m<-datos[p:f,2]
		total<-as.numeric(datos[p:f,3])
		total.h<-as.numeric(datos[p:f,4]) + as.numeric(datos[p:f,5]) + as.numeric(datos[p:f,6])
		total.m<-as.numeric(datos[p:f,7])+as.numeric(datos[p:f,8])+as.numeric(datos[p:f,9])
		salida<-as.data.frame(cbind(ind,n.m,total,total.h,total.m))
		salida[,3:5]<-apply(salida[,3:5],2,as.numeric)
		salida[,1]<-as.character(salida[,1])
		salida[,2]<-as.character(salida[,2])
		colnames(salida)<-c("cod","nombre","paro total","paro total hombres","paro total mujeres")
		salida[which(is.na(salida[,3])),3]<-0
		salida[which(is.na(salida[,4])),4]<-0
		salida[which(is.na(salida[,5])),5]<-0
		salida<-rbind(salida,rep(NA,5))
		nd<-dim(salida)[1]
		salida[nd,1]<-"Total"
		salida[nd,2]<-Cap
		salida[nd,3]<-sum(salida[1:nd-1,3])
		salida[nd,4]<-sum(salida[1:nd-1,4])
		salida[nd,5]<-sum(salida[1:nd-1,5])
		}
	salida
}