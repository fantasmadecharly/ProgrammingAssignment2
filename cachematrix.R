## Matrix inversion

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
		## Gets the matrix
        get <- function() x
		## Invert the matrix
        setinv <- function(solve) m <<- solve(x)
        ## Gets the inverted matrix
		getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
		## Gets from cache the inverse matrix 
		m <- x$getinv()
		## If matrix inverse exits in cache 
        if(!is.null(m)) {
                message("getting cached data")
                ## return the inverse matrix cached
				return(m) 
        }
		## Gets the matrix
        data <- x$get()
        ## Invert matrix
		m <- solve(data, ...)
		## Stores de inverse matrix in cache
        x$setinv(m)
        ## Return a matrix that is the inverse of 'x'
		m         
}
