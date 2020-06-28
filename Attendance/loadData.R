#R file to read the csv file and 
#return the data frame

loadData <- function(filename){

	#check the existense of the file
	if(file.exists(filename) == FALSE){
		stop("No file exists with this name in working directory")
	}
	
	#read the csv file
	atdn_df <- read.csv(fn)

	#return the data
	atdn_df

}