#this function draws a histogram
#of a given vector.

plotHistogram <- function(x,title,xlabs,ylabs,cols){

	mi <- as.integer(min(x))
	mx <- as.integer(max(x)+1)
	interval <- seq(mi,mx,1)
	#print(interval)
	hist(x,breaks=interval,freq=TRUE,main=title,xlab=xlabs,ylab=ylabs,xlim=range(interval),col=cols)

}