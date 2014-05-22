## This is a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        #1 of four functions
        #stores the user-added matrix to x
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        #2 of four functions
        get <- function() x
        
        #3 of four functions
        setinverse <- function(solve) m <<- solve
        
        #4 of four functions
        getinverse <- function() m

        # output is a list of four functions
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        #stores the matrix to m
        m <- x$getinverse()
        
        #checks if returned cache has anything in it
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        #if cache is empty store matrix to data
        data <- x$get()
        #get the inverse and store in local variable m
        m <- solve(data, ...)
        #store to the cache of object with user input
        x$setinverse(m)
        #return m or inverse of matrix
        m
}


