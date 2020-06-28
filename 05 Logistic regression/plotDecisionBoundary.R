#This function plots the decision boundary
#for a given theta, features and actual values

plotDecisionBoundary <- function(theta,X,y){

		
	#get any two random points of x1-feature one
	#like min-2 and max+2
	xp<-c(min(X[,2])-2,max(X[,2])+2)
	xp1<-t(as.matrix(xp))
	xp2<- ((-1)/theta[3]) * (theta[1] + (theta[2] * xp1))
	lines(xp1,xp2)	
	

}