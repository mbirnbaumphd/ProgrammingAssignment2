## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {

	inverse <- NULL

	#loads the input matrix
	inputMatrix <- function(pass) {
		x <<- pass
		inverse <<- NULL
	}

	#gets matrix
	get <- function() x

	#generates inverse
	setInverse <- function(invert) inverse <<- invert

	#retrieves inverse
	getInverse <- function() inverse

	#outputs inverse	
	list(inputMatrix = inputMatrix, get = get,
         setInverse = setInverse, getInverse = getInverse)	
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
	#get the inverse if already cached
	inverse <- x$getinverse()

	#If matrix is cached, state such
	if(!is.null(inverse)) {
		message("CACHED DATA.")
		return(inverse)
	}

	#get original matrix if not already cached
	data <- x$get()

	#compute inverse with solve function
	inverse <- solve(data)

	#cache the inverse result
	x$setinverse(inverse)

	#return inverse
	inverse
}
