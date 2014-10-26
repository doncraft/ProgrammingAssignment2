## The following functions assist to compute a matrix inversion, and cache
## the result for future use.

## The function below creates a matrix object and returns a list, defining
## 4 functions - get and set for matrix, and getSolve and setSolve for the
## calculate matrix inversion value.

makeCacheMatrix <- function(x = matrix()) {

	## assign s to hold a null placeholder value
	s <- NULL
	## create set function for matrix
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	
	## return matrix
	get <- function() x
	
	## assign solve value to s
	setSolve <- function(solve) s <<- solve

	## return solve value
	getSolve <- function() s

	## return list of 4 functions
	list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## This function returns a cache value for an inverse of the matrix or computes
## the inverse if no previous cache value is available.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	## get existing solve value
	s <- x$getSolve()
	
	## check to see if value exist.
	if (!is.null(s)) {
		message("getting cached data")
		return(s)
	}
	## if no pre-existing value
	data <- x$get()
	## calculate solve
	s <- solve(data)
	## set cache value for solve
	x$setSolve(s)
	## print solve value
	s
}
