# extrae informacion de los pdf de dgt


extrae <- function(file){
				data<-read.table(file,quote="\t",sep="\t",skip=1)
				a<-capture.output(data)	
				Seg<-matrix("A",8,2)
					for (i in 4:11){
						aux<-str_trim(substring(a[i],120,nchar(a[i])))
						aux<-as.matrix(unlist(strsplit(aux," ")))
						aux<-as.vector(aux[which(nchar(aux)!=0),])
						Seg[i-3,1]<-paste0(aux[1:(length(aux)-1)],collapse=" ")
						Seg[i-3,2]<-aux[length(aux)]
					}

				Censo.Cond<-matrix("A",3,2)
					for (i in 8:10){
						aux<-str_trim(substring(a[i],1,100))
						aux<-as.matrix(unlist(strsplit(aux," ")))
						aux<-as.vector(aux[which(nchar(aux)!=0),])
						if (i==8){
							Censo.Cond[i-7,1]<-paste0(aux[2:(length(aux)-1)],collapse=" ")
						} else {
							Censo.Cond[i-7,1]<-paste0(c("Conductores",aux[2]),collapse=" ")
						}
						Censo.Cond[i-7,2]<-aux[length(aux)]
					}

				sinITV<-matrix("A",3,2)
					for(i in 40:42){
						aux<-as.matrix(unlist(strsplit(a[i]," ")))
						aux<-as.vector(aux[which(nchar(aux)!=0),])
						sinITV[i-39,1]<-paste(str_trim(aux[2]),"sin ITV en vigor",collapse=" ")
						sinITV[i-39,2]<-str_trim(aux[3])
					}

				parque<-matrix("A",6,2)
				antiguedad<-matrix("A",6,2)
					for(i in 16:21){
						aux<-as.matrix(unlist(strsplit(a[i]," ")))
						aux<-as.vector(aux[which(nchar(aux)!=0),])
						if(i==18) {aux<-aux[1:5]}
						if(i==16) {aux<-c(aux[1],paste0(aux[2:4],collapse=" "),aux[5],"100",aux[6])}
						parque[i-15,1]<-paste0(c("Parque",aux[2]),collapse=" ")
						antiguedad[i-15,1]<-paste0(c("Antiguedad",aux[2]),collapse=" ")
						parque[i-15,2]<-str_trim(aux[3])
						antiguedad[i-15,2]<-str_trim(aux[5])
					}
				nn<-as.matrix(unlist(strsplit(a[2]," ")))
				nn<-as.vector(nn[which(nchar(nn)!=0),])
				nn<-nn[which(nn=="Municipio:"):(which(nn=="Provincia:")-1)]
				if(length(nn)>2){
					Nombre<-matrix(c(nn[1],paste0(nn[2:length(nn)],collapse=" ")),1,2)
					} else {
					Nombre<-matrix(c(nn[1:2]),1,2)
					}
				salida<-t(rbind(Nombre, parque, antiguedad, Censo.Cond, Seg))
				cols<-salida[1,]
				colnames(salida)<-cols
				#ifelse(k==1,colnames(salida)<-cols,colnames(salida)<-names(base))
				salida<-salida[-1,]
				salida<-as.data.frame(t(salida)) ; rownames(salida)<-NULL
				return(salida)
}