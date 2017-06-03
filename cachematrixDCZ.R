## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly. 
## This is a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{  
    # set base values
    inv <- NULL
    
    set <- function(y) 
    {
        x <<- y
        inv <<- NULL
    }
    
    # get function
    # Gets the matrix itself but not the inverse
    get <- function() x
    
    # Manually set the inverse
    setinverse <- function(inverse) inv <<- inverse
    
    # Get the inverse
    getinverse <- function() inv
    
    # Encapsulate into a list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)	
}


## Return a matrix that is the inverse of 'x'
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {

    
    # Get the current state of the inverse and see if it
    # has been computed yet
    inv <- x$getinverse()
    
    # If it has...
    if(!is.null(inv)) {
        # Simply return the computed inverse		
        message("getting cached matrix")
        return(inv)
    }
    
    # If it hasn't...
    # Get the matrix itself
    data <- x$get()
    
    # Find the inverse
    inv <- solve(data, ...)
    
    # Cache this result in the object
    x$setinverse(inv)
    
    # Return this new result
    inv    
}