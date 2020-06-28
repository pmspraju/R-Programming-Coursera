## This function give the hospital names 
## having given rank

## Input 
## argument 1 = either of Heart attack, Heard failure, Pneumonia
## argument 2 = rank

rankall <- function(outcome, num = "best") {
	## Read outcome data
	ocm <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	##validate the input state
	## Check that state and outcome are valid
	#get unique state codes
	ustcd <- unique(ocm$State)
	ustcd <- sort(ustcd,method="shell")
		
	if(!(outcome == "heart attack" | outcome == "heart failure" | outcome == "pneumonia"))
	{
		stop("invalid outcome") 
	}
	st<- 0
	sz <- dim(ocm)		
	opmatrix <- data.frame(num=rep(NA, 2), txt=rep("", 2),  # as many cols as you need
                 stringsAsFactors=FALSE) 
	names(opmatrix) <- c("hospital","state")
   
  for(st in 1:length(ustcd)){	
	tempmatrix <- matrix(0,ncol=sz[2],nrow=0)
	i <- 0
	h_v <- NULL
	#get the temp matrix having detalis of the selected state
	for(i in 1:sz[1])
	{
		if(ocm[i,7] == ustcd[st])
		{
			tempmatrix <- rbind(tempmatrix,ocm[i,])
		}
	}		
	
	## get the sorted values of death mortality rate of the hospitals for a given state and outcome
	if(tolower(outcome) == "heart attack")
	{	
		drn <-as.numeric(tempmatrix$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
		drns <- sort(drn,method = "quick")
		drnu <- unique(drns)			
		#drnsort <- order(drn)		
	}
	
	if(tolower(outcome) == "heart failure")
	{	
		drn <-as.numeric(tempmatrix$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
		drns <- sort(drn,method = "quick")
		drnu <- unique(drns)		
		#drnsort <- order(drn)			
	}

	if(tolower(outcome) == "pneumonia")
	{	
		drn <-as.numeric(tempmatrix$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
		drns <- sort(drn,method = "quick")
		drnu <- unique(drns)	
		#drnsort <- order(drn)	
	}
	 
	for(i in 1:length(drnu))
	{
		val <- drnu[i]
		ind <- which(drn == val)
		hospitals <- tempmatrix[ind,2]
		hospitals <- sort(hospitals,method = "shell")		 
		if(i ==1){
			h_v <- hospitals
		}else{
			h_v <- c(h_v,hospitals)
		}
	}
	if(is.na(drn))
	{
		ind <- is.na(drn)
		hospitals <- tempmatrix[ind,2]
		hospitals <- sort(hospitals,method = "shell")
		h_v <- c(h_v,hospitals)
	}
	if(num == "best")
	{
		rank <- 1
	} else if(num == "worst"){ rank <- length(h_v) 
	} else {
		rank <- as.numeric(num)
	}
	#opmatrix[st,1]<-h_v[rank]
	#opmatrix[st,2]<-ustcd[st]
	#rbind(opmatrix,c(h_v[rank],ustcd[st]))
	opmatrix[st, ] <- c(h_v[rank],ustcd[st])
  }
	opmatrix
}
