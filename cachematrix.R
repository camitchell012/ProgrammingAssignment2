## makeCacheMatrix creates a list of functions
## This function is used by cacheSolve to get/set the cahce

makeCacheMatrix <- function(x = matrix()) {
      
        # initialize the cache
        cache <- NULL
        
        # push the matrix to the parent environment
        set <- function(y) {
                x <<- y
                cache <<- NULL
        }
        
        # get the the matrix
        get <- function() x
        
        # invert the matrix and put it in cache
        setinverse <- function(inverse) cache <<- inverse
        
        # get the inverted matrix cache
        getinverse <- function() cache
        
        # return a list of functions to get/set matrix and its inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve inverts the matrix created in makeCacheMatrix
## If the inverted matrix already exists, return it
## Else, calculate the inverse and return it

cacheSolve <- function(x, ...) {
        
        ## try to get the inverted matrix in cache
        cache <- x$getinverse()
        
        # if cache is NoT null then just return from this function
        # with the cached matrix
        if(!is.null(cache)) {
                         message("getting cached data")
                         
                         # return the cache matrix
                         return(cache)
        }
        
        # retrieve the original matrix
        data <- x$get()
        
        # invert the original matrix
        cache <- solve(data, ...)
        
        # and then push this inverted matrix to the cache
        x$setinverse(cache)
        
        # return the matrix cache
        cache 
}
