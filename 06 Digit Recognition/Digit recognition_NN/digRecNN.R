#This function recoginze the input digit

digRecNN <- function(filename) {

	#load dependent functions
	source("loadData.R")
	source("displayData.R")	
	source("predict.R")
	
	#load data
	d <- loadData(filename)
	
	na <- names(d)
	dx <- d[[na[1]]]
	dy <- d[[na[2]]]
	
	#bind both matrixes in single one
	data <- cbind(dx,dy)

	#derive boundaries
	m <- dim(data)[1]
	p <- dim(data)[2]
	n <- p - 1
	
	#parse input data in to features and actual values
	X <- data[,1:n]
	y <- as.matrix(data[,p])
 
	#display random k digits from input data
	k <- 20
	displayData(X,y,k)

	#load provided weights for neural network
	weights <- loadData("ex3weights.mat")
	#print(names(weights))
	
	na <- names(weights)
	theta1 <- weights[[na[1]]]
	theta2 <- weights[[na[2]]]
	#print(dim(theta1))
	#print(dim(theta2))

	#get predicted values
	p_val <- predict(X,theta1,theta2)
	print(dim(p_val))

	#calculate % of equal values between actual and predicted values	
	per <- mean(p_val == y)*100
	print("Accuracy of the prediction:")
	print(per)
	
}