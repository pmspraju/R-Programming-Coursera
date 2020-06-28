#R File to plot the attendance
#details

plotAtdn <- function(filename){

	#load the loadData function
	source("loadData.R")
	source("selectOpt.R")
	source("plotEmp.R")
	source("calcTime.R")
	source("plotHistogram.R")
	
	data <- loadData(filename)

	data_names <- names(data)
	name_yes <- which(data_names == "Name")
	if(length(name_yes) == 0){
		stop("Data do not have a Name column")
	}
	
	#get all the names from the Name column
	emp <- levels(data$Name)

	#paste for user to select option
	for(i in 1:length(emp)){
		print(paste(i,".",emp[i]))
	}

	#Get the option from user
	msg<- "Select an employe(give Sno):"
	n <- as.integer(selectOpt(msg))
	#validate the entered value for integer
	while(is.na(n)){
		n <- as.integer(selectOpt(msg))
  	}
	#validate the entered value for valid range
	while(n < 1 || n > length(emp)){
		n <- as.integer(selectOpt(msg))
  	}

	print(paste("Selected employee:",emp[n]))
	selEmp <- emp[n]
	empInd <- which(data$Name == selEmp)
	empData <- data[empInd,]

	graphTypes <- c("Total Hours","In Time","Out Time","Work From Home")
	#paste for user to select option
	for(i in 1:length(graphTypes)){
		print(paste(i,".",graphTypes[i]))
	}
	
	#Get the option for graph type
	msg <- "Select a graph type:"
	n <- as.integer(selectOpt(msg))
	#validate the entered value for integer
	while(is.na(n)){
		n <- as.integer(selectOpt(msg))
  	}
	#validate the entered value for valid range
	while(n < 1 || n > length(graphTypes)){
		n <- as.integer(selectOpt(msg))
  	}
	selGraphType <- graphTypes[n]

	print(paste("Selected Graph:",selGraphType))	
	plotEmp(selEmp,empData,selGraphType)
	#empData
}