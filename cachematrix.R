
## This function creates a cache of the matrix.
## and return the list of the operations.

makeCacheMatrix <- function(x = matrix()) {
	matrix <- NULL
	set <- function(y) {
		x <<- y
		matrix <<- NULL
	}
	get <- function() x
	setInverse <- function(inv) matrix <<- inv
	getInverse <- function() matrix
	list(set = set, get = get,
		setInverse = setInverse,
		setInverse = setInverse)
}

## This function creates a cache of the inverse of the matrix 
## if it does not exist.

cacheSolve <- function(x, ...) {
	matrix <- x$getInverse()
	if(!is.null(matrix)) {
		message("getting cached data")
		return(matrix)
	}
	data <- x$get()
	matrix <- solve(data, ...)
	x$setInverse(matrix)
	matrix
}
