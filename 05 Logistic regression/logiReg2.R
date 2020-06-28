#This function is for logistic regression
#with regularization

logiReg2 <- function(filename){

	#check the existense of the file
	if(file.exists(filename) == FALSE){
		stop("No file exists with this name in working directory")
	}
	
	#load dependent functions
	source("mapFeature.R")
	source("costFunctionReg.R")	
	source("gradientReg.R")
	source("plotData.R")	
	source("plotDecisionBoundaryReg.R")
	source("predict.R")

	#load the data from the input filename
	d <- read.csv(filename)
	
	#plot the features
	xlm <- c(-1,1.5)
	ylm <- c(-1,1.5)
	xlb <- "Microchip Test 1"
	ylb <- "Microchip Test 2"
	plotData(d[,1],d[,2],d[,3],xlm,ylm,xlb,ylb)

	#function for legend
	legend("topright",c("y=1","y=0"),pch=c(3,20),col=c("black","yellow"))

	#Create matrix with plynomial features with nth degree
	deg <- 6
	fMat <- mapFeature(as.matrix(d[,1]),as.matrix(d[,2]),deg)
	#print(dim(fMat))

	#get the dimensions
	m <- (dim(fMat))[1]     #no of training examples
	n <- (dim(fMat))[2]     #no of features excluding column having actual values
	
	#Initialize fitting parameters	 
      initial_theta <- rep(0,n)

	#compute cost and gradient 
	X <- fMat
	y <- d[,3]

	#try with different lambda
	#lamda =0 - overfit curve. More accuracy, but not suitable for other datasets
	#lamda = 100 - underfit. less accuracy
	lambda <- 1
	C <- costFunctionReg(initial_theta,X,y,lambda)
	
	#print cost for initial theta
	print("Inital cost:" )
	print(C)

	#implement fminunc by usint trust pacakge in R
	op <- optim(initial_theta,costFunctionReg,gradientReg,X,y,lambda,method="BFGS",
			control = list(maxit=400))
	theta <- op$par
	print("trained parameters are:")
	print(theta)
	
	#plot decision boundary
	plotDecisionBoundaryReg(theta,X,y)	

	#predcit the output values using our trained parameters
	#on the training set
	pv <- predict(theta,X)
	pv <- as.matrix(pv)
	
	#calculate % of equal values between actual and predicted values	
	per <- mean(pv == y)*100
	print("Accuracy of the prediction:")
	print(per)
		
}