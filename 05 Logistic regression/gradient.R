#This function is used by optim function
#to retun gradient of the cost function

gradient <- function(thetal,X,y){

	#convert input theta vector in to matrix
	theta <- as.matrix(thetal)

	#make the column matrix in to row matrix
	theta <- t(theta)
	
	#calculate basic hypothesis function y = theta0 + theta1*x1 +..
	#t() gives transpose of a matrix
	hx <- X %*% t(theta) 

	#for logistic regression hypothesis function is sigmoid(h(x))
	#sigmoid is 1/(1+exp(-h(x))
	#sigmoid() is already written function in the workspace
	gx <- sigmoid(hx)

	#calculate gradient descent 
	diff <- gx - y
	Xt <- t(X)
	prod <- Xt %*% diff
	prod <- (1/(dim(X))[1]) * prod
	GD <- t(prod)
	GD
}