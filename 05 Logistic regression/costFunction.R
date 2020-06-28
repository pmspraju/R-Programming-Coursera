#This function calculates the cost
#of a given inputs
#input1 = Parameters 
#input2 = matrix having features,of training set, as columns.
#         first column will have constant one for theta0
#input3 = vector having actual values of the training set

costFunction <- function(thetal,X,y){
	
	#load dependent functions
	source("sigmoid.R")
	source("gradient.R")
	
	#convert input theta vector in to matrix
	theta <- as.matrix(thetal)

	#make the column matrix in to row matrix
	theta <- t(theta)

	#initialize cost to 0
	J <- 0
 	 	 
	#calculate basic hypothesis function y = theta0 + theta1*x1 +..
	#t() gives transpose of a matrix
	hx <- X %*% t(theta) 
	
	#for logistic regression hypothesis function is sigmoid(h(x))
	#sigmoid is 1/(1+exp(-h(x))
	#sigmoid() is already written function in the workspace
	gx <- sigmoid(hx)
	
	#calculate cost function value
	v1 <- (-1) * y
	v2 <- log(gx)
	v3 <- (1 - y)
	v41 <- (1 - gx)
	v4 <- log(v41)
	cv <- ((v1 * v2) - (v3 * v4))
	J <- sum(cv)/length(y)

	#calculate gradient descent 
	diff <- gx - y
	Xt <- t(X)
	prod <- Xt %*% diff
	prod <- (1/(dim(X))[1]) * prod
	GD <- t(prod)
	J
}