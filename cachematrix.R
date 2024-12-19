## Create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the inverse as NULL
    
    # Function to set the matrix
    set <- function(y) {
        x <<- y    # Set the matrix in parent environment
        inv <<- NULL  # Reset the cached inverse
    }
    
    # Function to get the matrix
    get <- function() x
    
    # Function to set the inverse
    setInverse <- function(inverse) {
        inv <<- inverse
    }
    
    # Function to get the inverse
    getInverse <- function() inv
    
    # Return list of methods
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Compute the inverse of the special matrix
cacheSolve <- function(x, ...) {
    # Try to get the cached inverse
    inv <- x$getInverse()
    
    # If the inverse is cached, return it with a message
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # If not cached, calculate the inverse
    data <- x$get()
    inv <- solve(data, ...)
    
    # Cache the inverse
    x$setInverse(inv)
    
    # Return the inverse
    inv
}
