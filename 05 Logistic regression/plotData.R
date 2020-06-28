plotData <- function(x1,x2,y,xlm,ylm,xlb,ylb) {
	
	#find the indices for +ve data
	y1 <- which(y==1)
	
	#find the indices for -ve data
	y0 <- which(y==0)

	#plot the +ve data
	plot(x1[y1],x2[y1], 		#data on cordinates
		xlim=xlm,
		ylim=ylm,
		col="black",		#color
		cex=1,			#size of icons
		pch=3,			#shape +
		xlab=xlb, 			#x label
		ylab=ylb			#y label
	)
	

	#plot the -ve data on the above plot(on which +ve data is plotted)
	points(x1[y0],x2[y0],col="yellow",cex=1.5,pch=20)	
	
	
} 