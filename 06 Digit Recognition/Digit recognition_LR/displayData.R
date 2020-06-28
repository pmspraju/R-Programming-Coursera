#This function accepts the pixel data
#and dimension n of n*n matrix, to 
#display n*n examples of the input data
#and display as a grid in image

displayData <- function(X,y,n) {

	source("flip.R")
	#develope the n*n matrix of pixels k*k 
	di <- dim(X)
	u<- di[1]
	v<- di[2]
	k<- sqrt(v)
	
	#select random n values out of 
	#total examples
	rv<- sample(u,n^2)
	
	Xs <- X[rv,]
 	if(is.matrix(Xs) == FALSE){
		Xs <- t(as.matrix(Xs))
	}

	ys <- as.matrix(y[rv,])
	
	i<-0
	os <- 0
	for (i in 1:n) {
		rw<-0
		jt<-0
		os <- n*(i-1)
		for (j in (1+os):(n+os)) {
			tv<-0
			tv<- Xs[j,]
			tv<-matrix(tv,k,k)
			tv<- rotate180.matrix(tv)
			tv<- rotate90.matrix(tv)
  
			av <- ys[j,1]
			if(jt==0){
				rw<-tv
				aw<-av
				jt<-j
			}else{
				rw<-rbind(rw,tv)
				aw<-cbind(aw,av)
			}
		}
		if(i==1){
			cw<- rw
			mw<- aw
		}else{
			cw<- cbind(cw,rw)
			mw<- rbind(mw,aw)
		}
	}
	print(flip.matrix(mw))
	#set parameters for grey scale
	cl<- grey(seq(0.5,1,length=12))
 
	#draw the grid using image function
	image(cw,col=cl,oldstyle=TRUE,useRaster=TRUE)
}