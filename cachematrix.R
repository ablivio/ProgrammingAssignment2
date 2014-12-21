## Matrix inversion is usually a costly computation.
## There may be some benefit to caching the inverse of a matrix rather than computing it repeatedly.
##
## The following pair of functions enable this through a special "matrix" object that manages a cache
## of its inverse.
##
## NOTE: these functions assume that the matrix supplied *is* invertible.

## makeCacheMatrix(value)
##   Creates a special "matrix" object initialized with 'value', with a cache.
##
## Object properties:
## - value = value of the object -- resets cache when set
## - cache = value of the cache -- NULL if not set
##
## Object methods:
## - set(m) / get(): getter & setter of the value
## - setcache(c) / getcache(): getter & setter of the cache

makeCacheMatrix <- function(value = matrix()) {
	
	## cache is initially unset
	cache <- NULL
	
	## value setter: records new value and resets cache
	set <- function(m) {
		value <<- m
		cache <<- NULL
	}
	
	## value getter
	get <- function() value
	
	## cache setter
	setcache <- function(c) {
		cache <<- c
	}
	
	## cache getter
	getcache <- function() cache
	
	list(
		set = set,
		get = get,
		setcache = setcache,
		getcache = getcache
	)
}


## cacheSolve(mat, ...)
##   Returns a matrix that is the inverse of 'mat'
##
## Computes the inverse of a special "matrix" returned by makeCacheMatrix(),
## leveraging its caching capability.
## If the cache has already been set it is assumed to hold the inverse of the matrix.
## Otherwise the inverse is computed and stored in the cache.

cacheSolve <- function(mat, ...) {
	
	## if cache is set we don't need to compute the inverse
	cache <- mat$getcache()
	if (!is.null(cache)) {
		message("getting cached data")
		inverse <- cache
	}
	else {
		## cache is not yet set -- let's compute the inverse
		value <- mat$get()
		inverse <- solve(value)
		mat$setcache(inverse)
	}
	inverse
}

