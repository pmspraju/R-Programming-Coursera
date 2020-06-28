#This function plots the decision boundary
#for a given theta, features and actual values

plotDecisionBoundaryReg <- function(theta,X,y){
		
	#get the grid range
	u <- seq(-1,1.5,length=50)
	v <- seq(-1,1.5,length=50)

	#calculate values to be plot	
	z <- matrix(0,nrow=length(u),ncol=length(v))
	i<-0
	j<-0
	thetaM <- as.matrix(theta)
	for (i in 1:length(u)){
		for (j in 1:length(v)){
			ft <- mapFeature(as.matrix(u[i]),as.matrix(v[j]),6)
			z[i,j] <- ft %*% thetaM
		}
	}
	#z <- t(z)
	#contour plot
	#u= x axis;v= y axis; z = points to plot
	#add = TRUE makes the contour plot added on 
	#another previous plot
	contour(u,v,z,levels=c(0),add = TRUE)	

}