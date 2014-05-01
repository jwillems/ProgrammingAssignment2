## Put comments here that give an overall description of what your
## functions do

## This function checks if the matrix is changed - if not it will get the latest matrix else it will set a new matrix.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## This function computes the inverse of a matrix using makeCacheMatrix.
## If the matrix was changed is stores the new matrix using the above function and computes a new inverse.
## Else it will obtain the previous result.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if(!is.null(i)) {
			message("getting cached data")
			return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}
