#calculate the mean of the data captured from a file at the path
#mentioned in the variable directory
#
complete <- function(directory, id = 1:332) {
	accumulate_val <- 0
	ind <- 0
	m <- matrix(0, nrow=length(id), ncol=2)
	colnames(m) <- list("id","nobs")
	print(id)
	for(i in id){
		
		#build the absolute path of file using id
		if(nchar(i) == 1){
			filePath <- paste(directory,"/00",i,".csv",sep="")
		} else if (nchar(i) == 2) {
			filePath <- paste(directory,"/0",i,".csv",sep="")
		} else{
			filePath <- paste(directory,"/",i,".csv",sep="")
		}
		#check the existence of the file
		if(file.exists(filePath) == FALSE){
			print("File does not exists with given id")
			break
		}
		#read data 
		data<- read.csv(filePath)
		
		#create a vector with 1 if both values are present and with 0 otherwise
		nobs_vector = apply(data,1,function(x){if( !is.na(x[2]) && !is.na(x[3]) ) {1} else {0}})

		#calculate length of 1 in above vector
		nobs_count = length(which(nobs_vector == 1))
		ind <- ind + 1
		m[ind,1] <- i
		m[ind,2] <- nobs_count
	}
	#print(m)
	m
}

 
