#This function recoginze the input digit

digRec <- function(filename) {

	#load dependent functions
	source("loadData.R")
	source("displayData.R")	
	source("oneVsAll.R")
	source("predictOneVsAll.R")

	#load data
	data<- loadData(filename)

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
	
	#call function to get trained parameters for the training set
	num_labels <- 10
	lambda <- 0.1
	all_theta <- oneVsAll(X,y,num_labels,lambda)
	#print(dim(all_theta))

	#call funtion to predict the values
	pVal <- predictOneVsAll(all_theta,X)
	#print(dim(pVal))

	#calculate % of equal values between actual and predicted values	
	per <- mean(pVal == y)*100
	print("Accuracy of the prediction:")
	print(per)
	
	#select any one index out of 5000
     	sInd <- sample(1:m,1)
     	print("Actual values is:")
     	print(y[sInd])

     	#calculate predicted value
     	tMat <- t(as.matrix(X[sInd,]))
     	tMat <- cbind(1,tMat)
     	pVec <- tMat %*% t(all_theta)
     	pV <- which(pVec == max(pVec))
     	print("Predicted value is:")
     	print(pV)

}