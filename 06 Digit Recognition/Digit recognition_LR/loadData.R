#This function accepts the file name
#and load the data in a matlab file

loadData <- function(filename){

	#check the existense of the file
	if(file.exists(filename) == FALSE){
		stop("No file exists with this name in working directory")
	}

	#To read a matlab file, package
	#R.matlab is needed. Load the 
	#library R.matlab
	library(R.matlab)

	#This data has two matrices
	#matrix 1 - have 5000 examples, each having data pixels related to a digit
	#dim(matrix 1) = 5000,400
	#matrix 2 - have 50000 examples, having actual number of the digit in the
	#matrix 1. 
	#dim(matrix 2) = 5000,1
	d <- readMat(filename)
	
	dx <- d$X
	dy <- d$y
	
	#bind both matrixes in single one
	dxy <- cbind(dx,dy)

	#return combined matrix
	dxy
		

}