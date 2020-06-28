#This function recoginze the input digit

digRecNNF <- function(filename) {

	#load dependent functions
	source("loadData.R")
	source("displayData.R")	
	#source("predict.R")
	
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

}