## These two functions establish a CacheMatrix object and give it
## its own version of the solve() function for matrices.

## This creates a Cache Matrix object, which is essentially a matrix
## wrapped in a larger object to have a cache to store its inverse.

makeCacheMatrix <- function(x = matrix()) {
        x_inv <- NULL
        set <- function(y) {
            x <<- y
            x_inv <<- NULL
        }
        get <- function() x
        setinverse <- function(x_inverse)  x_inv <<- x_inverse
        getinverse <- function() x_inv
        list( set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}

## This takes a CacheMatrix object (and only a CacheMatrix object),
## and either pulls its inverse from the object's cache or calculates
## and caches the inverse of the matrix if it has not been cached.

cacheSolve <- function(y, ...) {
        ## Return a matrix that is the inverse of 'x'
    x_inv <- y$getinverse()
    if(!is.null(x_inv)) {
        message("getting cached data")
        return(x_inv)
    }
    data <- y$get()
    x_inv <- solve(data, ...)
    y$setinverse(x_inv)
    x_inv
}
