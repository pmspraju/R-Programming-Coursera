#This function uses given
#weights and predicts the values

predict <- function(X,Theta1,Theta2){
	
	source("sigmoid.R")

	#add interceptor column
	m <- dim(X)[1]	
	oneCol <- matrix(1,nrow=m,ncol=1)
	X <- cbind(oneCol,X)	
	
	#product of input data and 
	#transpose of trained parms gives
	#hypothesis function h(x) values
	hid_hx <- X %*% t(Theta1)

	#sigmoid of hypothesis gives 
	#actual values
	hid_val <- sigmoid(hid_hx)
	#print(dim(hid_val))
	
	#add interceptor column for hidden
	#layer matrix
	m <- dim(hid_val)[1]	
	oneCol <- matrix(1,nrow=m,ncol=1)
	hid_val <- cbind(oneCol,hid_val)

	#product of hidden layer parms
	#and theta2 will give hypo func values
	out_hx <- hid_val %*% t(Theta2)
	out_val <- sigmoid(out_hx)
	#print(dim(out_val))
	
	#use apply function to get max value of every row
	maxVal <- apply(out_val,1,function(x){a<-max(x); which(x ==a)})
	maxValmat <- as.matrix(maxVal)	
	maxValmat
}