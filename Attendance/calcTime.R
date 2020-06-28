#Function reads a list of values for time
#in the format  hh:mm and gives out a list
# of times in hours

calcTime <- function(timeList){

	outTime <- lapply(as.vector(timeList), function(x) {
						if(!is.na(x) && x > " "){
							s <- unlist(strsplit(x, split=":"))
							hrs <- as.numeric(s[1])
							min <- as.numeric(s[2])
							thrs <- hrs + (min/60)
							as.numeric(thrs)
						}else{0}
					    }
				)
	res <- as.numeric(outTime)
	res
}