#This function create polynomial features
#on nth degree for the given input features

mapFeature <- function(x1,x2,deg){

	#create a matrix with one column and
	#rows is of size equal to input feature
	polMat <- as.matrix(rep(1,dim(x1)[1]))

	i<-0
	j<-0
	for (i in 1:deg){
		for (j in 0:i){
			col <- (x1^(i-j)) * (x2^j)
			polMat <- cbind(polMat,col)
		}
	}
	polMat
}