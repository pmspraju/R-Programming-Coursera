## This function reads the data related to hospital and 
## gives hospital names with lowest value of the parameters

## Input 
## argument 1 = State code
## argument 2 = either of Heart attack, Heard failure, Pneumonia

best <- function(state, outcome = "heart attack") {

	##read outcome-of-care-measures csv file
	ocm <- read.csv("outcome-of-care-measures.csv", colClasses = "character")	

	##validate the input state
	#get unique state codes
	ustcd <- unique(ocm$State)
	lstcd <- length(which(ustcd == toupper(state)))
	if(lstcd == 0)
	{ 
		stop("invalid state") 
		
	}
	
	if(!(outcome == "heart attack" | outcome == "heart failure" | outcome == "pneumonia"))
	{
		stop("invalid outcome") 
	}
	
	sz <- dim(ocm)
	i <- 0
	tempmatrix <- matrix(0,ncol=sz[2],nrow=0)
	
	#get the temp matrix having detalis of the selected state
	for(i in 1:sz[1])
	{
		if(ocm[i,7] == toupper(state))
		{
			tempmatrix <- rbind(tempmatrix,ocm[i,])
		}
	}		
	
	## get the minimum value of death mortality rate of the hospitals for a given state and outcome
	if(tolower(outcome) == "heart attack")
	{	
		minval <- min(as.numeric(tempmatrix$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),na.rm = TRUE)
		ind <- which(as.numeric(tempmatrix$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) == minval)		
	}
	
	if(tolower(outcome) == "heart failure")
	{	
		minval <- min(as.numeric(tempmatrix$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),na.rm = TRUE)
		ind <- which(as.numeric(tempmatrix$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) == minval)		
	}

	if(tolower(outcome) == "pneumonia")
	{	
		minval <- min(as.numeric(tempmatrix$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),na.rm = TRUE)
		ind <- which(as.numeric(tempmatrix$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) == minval)		
	}
	
	#get the hospitals name as a vector
	besthospitals <- tempmatrix[ind,2]		
	sl <- sort(besthospitals, method = "shell")
	sl
}