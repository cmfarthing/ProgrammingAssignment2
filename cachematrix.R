## Store the inverse of a matrix into cache memory to be used later without
## needing to recalculate the inverse. 

## Creates an R object that stores a matrix and its inverse. An argument of 
## makeCacheMatrix is required for use in cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
        #initialize inverse matrix m as NULL
        m<-NULL 
        set<-function(y){
                x<<-y
                m <<- NULL
        }
        get<-function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)

}


## Requires an argument returned by makeCacheMatrix.  This functions
## will check to see if the inverse of the matrix is already stored in cache.
## If so, cached inverse matrix will be returned. If not, the inverse of the
## matrix will be calculated, returned, and stored to the cached data. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        Matrix <- x$get()
        m <- solve(Matrix)
        x$setinverse(m)
        m
}
