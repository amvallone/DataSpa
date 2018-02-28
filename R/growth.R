
# Compute the groth rate of n periods od an x variable

growth<-function(x,y,n){
	z<-(((x/y)^(1/n))-1)*100
	z
	}