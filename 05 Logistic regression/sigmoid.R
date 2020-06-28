#this function calculates sigmoid of a given input.
#if input is a matrix, result is a matrix having 
#sigmoid of every element

sigmoid <- function(m){
	
	i<-0
	j<-0

	#change input in to matrix
	mtx <- as.matrix(m)
	m_size <- dim(mtx)
	
	#create output matrix with zeros
	sig_vec <- matrix(0,nrow=m_size[1],ncol=m_size[2])

	#traverse the input matrix
	for(i in 1:m_size[1])
	{
		for(j in 1:m_size[2])
		{
			#get the sigmoid function value
			sig_vec[i,j] <- 1 / (1 + exp(-mtx[i,j]))		
		}
	}
	sig_vec
}