
makeCacheMatrix <- function(x = matrix()) {
        cachedInverse <- NULL
        set <- function(y) {
                x <<- y
                cachedInverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) cachedInverse <<- inverse
        getInverse <- function() cachedInverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}



cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	inverseMatrix <- x$getInverse()
        	if(!is.null(inverseMatrix)) {
                	message("getting cached data")
                	return(inverseMatrix )
        	}
     	data <- x$get()
     	inverseMatrix <- solve(data, ...)
      x$setInverse(inverseMatrix)
      inverseMatrix
}
