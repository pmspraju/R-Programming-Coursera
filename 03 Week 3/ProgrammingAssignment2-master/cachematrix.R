## This script is to accept matrix and stores in cache.

## This function takes a matrix as input and
##generates an object which can be used to 
##get and set values 

makeCacheMatrix <- function(x = matrix()) {

	## Initial null value in inverse
	inv <- NULL 

	##set function of the input matrix
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

	##get function of the input matrix
	get <- function() x

	##set function to set inverse value once it is calculated
	setinv <- function(inverse) inv <<- inverse

	##get function to get the already set inverse value
	getinv <- function() inv

	##setting function names
	list(set = set, get = get, 
	     setinv = setinv,
	     getinv = getinv)

}


## This function verifies cache for stored inverse value.
##if yes, it returns the value from cache
## if not, it calculates and then stores the value to cache

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'

	  ##get the inverse from cache if available
	  inv <- x$getinv()

	  ## if cache has value show it and return
	  if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }	

	  ## if cache donot have a value then get input matrix
	  data <- x$get()

	  ##calculate inverse
	  inv <- solve(data)

	  ##set the inverse value to cache
	  x$setinv(inv)

	  ##return inverse value
	  inv
}
