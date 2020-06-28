#This function predicts the accuracy
#of the training set between the
#actual values and values calculated
#using trained parameters

predict <- function(thetaV,X){

	#convert theta vector in to matrix
	theta <- as.matrix(thetaV)	

	#calculate basic hypothesis function y = theta0 + theta1*x1 +..
	#t() gives transpose of a matrix
	hx <- X %*% theta

	#for logistic regression hypothesis function is sigmoid(h(x))
	#sigmoid is 1/(1+exp(-h(x))
	#sigmoid() is already written function in the workspace
	gx <- sigmoid(hx)
	
	#if probability is >= 0.5 then positive result
	#else negative result
	pos <- which(gx >= 0.5)
	neg <- which(gx < 0.5)

	#develop matrix with 1 and 0
	pm <- rep(0,dim(X)[1])
	pm[pos] <- 1
	pm[neg] <- 0

	#return predicted values
	pm
}