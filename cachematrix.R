## R Programming assignment #2

## Creates a special "matrix" which caches its inverse for faster access
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # Setter and getter for matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    
    # Setter and getter for cached matrix inverse
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    
    # Create list with all the accessor functions and return it
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Calculates inverse of the matrix and caches it for future 
cacheSolve <- function(x, ...) {
    # Check for the cached value
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Calculate inverse from the matrix
    data <- x$get()
    inv <- solve(data, ...)
    # ... and cache it
    x$setinv(inv)
    
    # Return the matrix inverse
    inv
}
