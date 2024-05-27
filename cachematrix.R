## makeCacheMatrix is a function that returns a list of functions to interact with our matrix and its inverse. It allows us to set and get the matrix and its inverse.
## cacheSolve checks if the inverse is already cached. If it is, it retrieves it; if not, it calculates the inverse, caches it, and then returns it.
## This setup ensures that the inverse is only calculated when necessary, saving computation time if the inverse is requested multiple times.

## Write a short comment describing this function
    # This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
}

    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function
    # This function computes the inverse of the "matrix" returned by makeCacheMatrix above.
    # If the inverse has already been calculated (and the matrix has not changed), then
    # cacheSolve should retrieve the inverse from the cache.

    cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
