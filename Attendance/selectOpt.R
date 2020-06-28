#prompts user to select employee

selectOpt <- function(msg){
	n<-0
	n <- readline(prompt=msg)
	n <- as.integer(n)
}