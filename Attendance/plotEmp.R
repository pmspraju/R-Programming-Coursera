#process the specific employee data

plotEmp <- function(empName,empData,selGraphType){
	
	#get levels of status
	status_list <- levels(empData$Status)	
	names(empData) <- sub(" ", ".", names(empData))
	present_ind <- which(empData$Status == "Present")

	if(selGraphType == "Total Hours"){	
		#set graphical parameters
		old.par <- par(mfrow=c(2, 1))
	
		#Draw pattern
		tit <- paste(empName,"Total Hours plot")	
		xy_list <- list(x = empData$Date[present_ind],y = empData$Total.Hours[present_ind])
		x <- as.Date(empData$Date[present_ind],format="%d-%b-%y")
		y <- as.character(empData$Total.Hours[present_ind])
		y <- as.numeric(y)
		avg_hrs <- mean(y)
		plot(x,y,ylim=range(y), col = "blue",cex=0.8,type = "p", main = tit,xlab="Dates",ylab="Total Hours")
		abline(h = 9.15, v = 0, col = "green")
		if(avg_hrs >= 9.15){
			abline(h = avg_hrs, v = 0, col = "blue")	
		}else{
			abline(h = avg_hrs, v = 0, col = "red")				
		}
		
		#Draw Histogram
		tit <- "Total-Hours count"
		xlabs <- "Total-Hours in Hrs"
		ylabs <- "Count of In-time#"
		cols <- c("#ABABAB","#545454")		
		plotHistogram(y,tit,xlabs,ylabs,cols)
		
		#set old parameters
		par(old.par)

	}
	
	if(selGraphType == "In Time"){
		#set graphical parameters
		old.par <- par(mfrow=c(2, 1))
	
		#draw pattern
		tit <- paste(empName,"In-time pattern")
		x <- as.Date(empData$Date[present_ind],format="%d-%b-%y")
		z <- calcTime(empData$In.Time[present_ind])
		avgInTime = mean(z)
		miot <- as.integer(min(z))
		mxot <- as.integer(max(z) + 1)
		smoothingSpline = smooth.spline(x, z, spar=0.8,all.knots = TRUE) #all.knots = TRUE
		plot(x,z,ylim=range(miot,mxot), col = "blue",cex=0.4,type = "p", main = tit,xlab="Dates",ylab="In-Time")
		lines(smoothingSpline,col="blue",lwd=2)	
		if(avgInTime <= 10){
			abline(h = avgInTime , v = 0, col = "blue",lty=3)	
		}else{
			abline(h = avgInTime , v = 0, col = "red",lty=3)	
		}
		
		#Draw Histogram
		tit <- "In-Time count"
		xlabs <- "Swipe-in Time in Hrs"
		ylabs <- "Count of In-time#"
		cols <- c("#ABABAB","#545454")				
		plotHistogram(z,tit,xlabs,ylabs,cols)
		
		#set old parameters
		par(old.par)
	}

	if(selGraphType == "Out Time"){
		#set graphical parameters
		old.par <- par(mfrow=c(2, 1))
	
		#draw pattern
		tit <- paste(empName,"Out-time pattern")
		x <- as.Date(empData$Date[present_ind],format="%d-%b-%y")
		z <- calcTime(empData$Out.Time[present_ind])
		avgInTime = mean(z)
		miot <- as.integer(min(z))
		mxot <- as.integer(max(z) + 1)
		smoothingSpline = smooth.spline(x, z, spar=0.8,all.knots = TRUE) #all.knots = TRUE
		plot(x,z,ylim=range(miot,mxot), col = "blue",cex=0.4,type = "p", main = tit,xlab="Dates",ylab="Out-Time")
		lines(smoothingSpline,col="blue",lwd=2)	
		if(avgInTime <= 20){
			abline(h = avgInTime , v = 0, col = "blue",lty=3)	
		}else{
			abline(h = avgInTime , v = 0, col = "red",lty=3)	
		}
		
		#Draw Histogram
		tit <- "Out-Time count"
		xlabs <- "Swipe-Out Time in Hrs"
		ylabs <- "Count of Out-time#"
		cols <- c("#ABABAB","#545454")				
		plotHistogram(z,tit,xlabs,ylabs,cols)
		
		#set old parameters
		par(old.par)
	}
	
	if(selGraphType == "Work From Home"){
		statusLevels <- unique(empData$Status)		
		hl <- c()
		for(i in 1:length(statusLevels)){
			ck <- grep("home",statusLevels[i],ignore.case=TRUE)
			if(length(ck) > 0){
				hl <- c(hl,statusLevels[i])
			}
		}
		if(length(hl) >0){
			print("Under Construction")
		}else{
			print(paste("No Work From Home records for", empName))
		}
	}	
}