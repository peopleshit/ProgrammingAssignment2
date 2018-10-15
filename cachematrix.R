## The functions reperesented below do the caching and
## inverse of a generic matrix.

## Function is responsible for object creation that is capable
## of caching matrix object and its inverse.

makeCacheMatrix <- function(mx = matrix()) {
	imx <- NULL
	
	get <- function() return(mx)
	
	getInverse <- function() return(imx)
	
	set <- function(val) {
		mx <<- val
		inverse <<- NULL
	}
	
	setInverse <- function(val) imx <<- val
	
	return(
		list(
			set = set,
			get = get,
			setInverse = setInverse,
			getInverse = getInverse
		)	
	)
}


## Function gets inverse from cached matrix.
## In case cached matrix is null null is returned.
## If inverted matrix is not present in cache, its
## value is calculated and cached.

cacheSolve <- function(cmx, ...) {
	imx <- cmx$getInverse()
	
	if (!is.null(imx)) return(imx)
	
	mx <- cmx$get()
	imx <- solve(mx, ...)
	cmx$setInverse(imx)
	
	return(imx)
}
