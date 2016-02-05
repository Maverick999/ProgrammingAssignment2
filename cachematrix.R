## The functions makeCacheMatrix takes a matrix input and stores it as a 
## list. The function cacheSolve finds the inverse of the matrix entered in 
## makeCacheMatrix. However, before finding the inverse function, cacheSolve
## checks if the inverse was already found, and if so, returns the inverse 
## matrix from the cache.

## This function takes a matrix input and stores it as a list of four functions
## that sets the matrix, gets the matrix, sets the inverse of the matrix and 
## gets the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
           m<-NULL    
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinverse = setinv,
             getinverse = getinv)
        
}


## This function takes the list created in makeCacheMatrix
## and finds the inverse of the matrix created by makeCacheMatrix.
## However, it first checks to see if the inverse matrix has already been found. 
## If so, it `gets' the inverse matrix from the cache and skips the computation. 
## Otherwise, it finds and returns the inverese matrix and sets the inverse matrix
## in the cache via the `setinv`.

cacheSolve <- function(x, ...) {
        
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
