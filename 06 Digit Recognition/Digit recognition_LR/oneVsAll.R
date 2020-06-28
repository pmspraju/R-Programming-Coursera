#This function calls the costfunctions
#and iterates in a loop to classify
#multi class parameters

oneVsAll <- function(X,y,num_labels,lambda){

	#load dependent functions
	source("lrCostFunction.R")
	source("lrGradient.R")

	#no of training examples
	m <- dim(X)[1]

	#no of features
	n <- dim(X)[2]	

	#initialize theta matrix
	all_theta <- matrix(0,nrow=num_labels,ncol=n+1)

	#add interceptor column
	oneCol <- matrix(1,nrow=m,ncol=1)
	X <- cbind(oneCol,X)

	#load library of Rcgmin
	require(numDeriv)
	require(Rcgmin)
	
	#loop on no of labels;Theta vector for each label
	for (i in 1:num_labels){
		initial_theta <- rep(0,n+1)
		temp_theta <- rep(0,n+1)
		xDer <- X
		yDer <- matrix(0,nrow=m,ncol=1)
		#for logistic regression, output value should be
		#1 or 0. accordingly change actual value vector
		for (j in 1:m){
			if(y[j] == i)
				{yDer[j] <- 1}
			else  {yDer[j] <- 0}
		}
		#lower bound values for theta
		lower <- rep(-Inf,n+1)

		#upper bound values for theta
		upper<-rep(10,n+1) # to get arrays set

		#for unconstrained calculation, keep as 1. each 1 for each value in 
		#theta vector
		bdmsk<-rep(1,n+1)

		#call Rcgmin function to get optimized theta values
		op <- Rcgmin(initial_theta,lrCostFunction,lrGradient,lower,upper,bdmsk,
				 control=list(maxit=50,trace=0),xDer,yDer,lambda)

		#optim function  does the same thing but, not as accurate as rcgmin
		#op <- optim(initial_theta,lrCostFunction,lrGradient,xDer,yDer,lambda,
		#		method="BFGS",control = list(maxit=50))
		
		#construct matrix add all theta vectors for each label
		if(i==1){
			all_theta <- rbind(op$par)
		}else{
			all_theta <- rbind(all_theta,op$par)
		}
	}
	all_theta	
}