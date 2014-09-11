## The purpose of this program is to create two functions to cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function( a = matrix() ) {
    b <- NULL
    set <- function( matrix ) {
            a <<- matrix
            b <<- NULL
    }
    get <- function() {
    	a
    }
    setInverse <- function(inverse) {
        b <<- inverse
    }
    getInverse <- function() {
        b
    }
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
    a <- x$getInverse()
    if( !is.null(a) ) {
            message("getting cached data")
            return(a)
    }
    data <- x$get()
    a <- solve(data) %*% data
    x$setInverse(a)
    a
