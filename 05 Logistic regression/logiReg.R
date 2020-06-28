#This function load the training set in the
#workspace and implement logistic regression

logiReg <- function(filename){

	#check the existense of the file
	if(file.exists(filename) == FALSE){
		stop("No file exists with this name in working directory")
	}
	
	#load dependent functions
	source("costFunction.R")	
	source("plotData.R")	
	source("plotDecisionBoundary.R")
	source("predict.R")

	#load the data from the input filename
	d <- read.csv(filename)
	
	#plot the features
	xlm <- c(30,100)
	ylm <- c(30,100)
	xlb <- "Exam 1 Score"
	ylb <- "Exam 2 Score"
	plotData(d[,1],d[,2],d[3],xlm,ylm,xlb,ylb)

	#function for legend
	legend("topright",c("Admitted","Not Admitted"),pch=c(3,20),col=c("black","yellow"))
	

	#get the dimensions
	m <- (dim(d))[1]     #no of training examples
	n <- (dim(d))[2] - 1 #no of features excluding column having actual values
	
	#extract features only in to a matrix
	f <- d[,1:n]	

	#create a matrix with all 1 as first column + features
	X <- matrix(1,nrow=m,ncol=n+1)
	X <- cbind(X[,1],f)
	X <- as.matrix(X)
	#create a vector having actual values
	y <- d[,(n+1)] 
	
	#Initialize fitting parameters
	#initial_theta <- matrix(0,nrow=1,ncol=n+1)
	 initial_theta <- rep(0,n+1)

	#compute cost and gradient 
	C <- costFunction(initial_theta,X,y)
	
	#print cost for initial theta
	print("Inital cost:" )
	print(C)
	 
	#implement fminunc by usint trust pacakge in R
	op <- optim(initial_theta,costFunction,gradient,X,y,method="BFGS",
			control = list(maxit=400))
	theta <- op$par
	print("trained parameters are:")
	print(theta)
	
	#plot decision boundary
	plotDecisionBoundary(theta,X,y)	

	tv <- matrix(c(1,45,85),nrow=1,ncol=3)
	prob <- sigmoid(tv %*% as.matrix(theta)) * 100
	print("Student with x1=45, x2=85 will have probability:")
	print(prob)

	#predcit the output values using our trained parameters
	#on the training set
	pv <- predict(theta,X)
	pv <- as.matrix(pv)
	
	#calculate % of equal values between actual and predicted values	
	per <- mean(pv == y)*100
	print("Accuracy of the prediction:")
	print(per)
}