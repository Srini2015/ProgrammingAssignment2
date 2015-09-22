## MakeCacheMatrix function stores the Functions
## Creates a matrix object which can cache its inverse using Solve function
## Convert the Matrix to a list Using as.list and store it in x
## Restore the value of M to Null so that old value can be replaced

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
            x <<- as.list(y)
            m <<- NULL
    	}
	get <- function() x
	setsolve <- function(solve) m <<- solve
	getsolve <- function() m
	 list(set = set, get = get,
		setsolve = setsolve,
		getsolve = getsolve)
}

## cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix 
## First checks if M value is already present, if present provides it value else calculates it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
