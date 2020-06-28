#calculate the mean of the data captured from a file at the path
#mentioned in the variable directory
#
pollutantmean <- function(directory, pollutant, id = 1:332) {
	accumulate_val <- 0
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
		val <- data[pollutant]
		#remove na values
		ind_na <- is.na(val)
		val_no_na <- val[!ind_na]
		if(i == id[1]){
			accumulate_val <- val_no_na
		}else {
			accumulate_val <- c(accumulate_val,val_no_na)
		}
	}
	#print(accumulate_val)
	mean(accumulate_val)
}

 
