## Creates a CacheMatrix with our provided matrix, x. 
makeCacheMatrix <- function(x = matrix()) {
	## Used to cache our inverted matrix
	i <- NULL
	## Cache a new matrix
	set <- function(y) {
		x <<- y
		## Note that we need to remove any old inversion
		i <<- NULL
	}
	## Returns the cached matrix
	get <- function() x
	## Caches our inverse
	setinverse <- function(inverse) i <<- inverse
	## Returns the cached inverted matrix
	getinverse <- function() i
	list(set = set, get = get,
			 setinverse = setinverse,
			 getinverse = getinverse)
}

## Solves (inverts) a CacheMatrix, x. 
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	## If we have calculated the inverse earlier we can return the cached result
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	## At this point our i is still NULL and needs to be calculated
	## data is our original matrix
	data <- x$get()
	## solves our cached matrix to get our inverse
	i <- solve(data, ...)
	## cache our inverted matrix so that we do not need to do this caculation again
	x$setinverse(i)
	## returns our inverted matrix
	i
}
