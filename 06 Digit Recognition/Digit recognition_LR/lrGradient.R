#This function is used by optim function
#to retun gradient of the cost function
#with regularization parameter

lrGradient <- function(thetal,X,y,lambda){

	#convert input theta vector in to matrix
	theta <- as.matrix(thetal)

	#make the column matrix in to row matrix
	#if(dim(X)[2] == dim(theta)[2])
	#	{theta <- t(theta)}
	
	#calculate basic hypothesis function y = theta0 + theta1*x1 +..
	#t() gives transpose of a matrix
	hx <- X %*% theta 

	#for logistic regression hypothesis function is sigmoid(h(x))
	#sigmoid is 1/(1+exp(-h(x))
	#sigmoid() is already written function in the workspace
	gx <- sigmoid(hx)

	#calculate gradient descent 
	diff <- gx - y
	Xt <- t(X)
	prod <- Xt %*% diff
	prod <- (1/(dim(X))[1]) * prod
	
	#Get parameters and replace first element with zero
	#as we do not regularize parameter related to
	#interceptor column
	thetaReplaced <- theta
	thetaReplaced[1] <- 0
	thetaReplaced <- (lambda/length(y)) * thetaReplaced
	#thetaReplaced <- t(thetaReplaced)
	grad <- prod + thetaReplaced
	
	GD <- t(grad)
	GDV <- as.numeric(GD)
	GDV
}