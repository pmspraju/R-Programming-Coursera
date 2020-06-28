#This functions takes trained
#parameters set and trained set
#examples and predicts the output
#values of the hypothesis function

predictOneVsAll <- function(all_theta,X){
	
	#add interceptor column
	m <- dim(X)[1]	
	oneCol <- matrix(1,nrow=m,ncol=1)
	X <- cbind(oneCol,X)
	
	prd <- X %*% t(all_theta)
	
	maxVal <- apply(prd,1,function(x){a<-max(x); which(x ==a)})
	maxValmat <- as.matrix(maxVal)	
	maxValmat
}