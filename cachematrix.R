## This program is to cache the inverse of a matrix. I don't know why you would want to do this
## but I assume there is some benefit to the process rather than computing it repeatedly.
## The following are a set of functions that are used to create a special object that
## stores a matrix and caches its inverse.

## The "makeCacheMatrix" This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## Initialize m to NULL
    m <- NULL
    ## creates the matrix, set
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## get will be the value of the matrix
    get<-function() x
    
    ## invert the matrix and store in m
    setmatrix<-function(solve) m<<- solve
    
    ## get the inverted matrix from m
    getmatrix<-function() m
    
    ### returns the created functions
    list(set=set, get=get,
    setmatrix=setmatrix,
    getmatrix=getmatrix)
}

## The cacheSolve function will compute the inverse of the special matrix returned by makeCacheMatrix
## in the above code. If the inverse has already been calculated (and the matrix has not changed), then
## the cacheSolve function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    ## Gets the inverse matrix from the cache if it exixts.
    m <- x$getmatrix(x)
    
    ## If the matrix inverse has already been calculated
    if(!is.null(inv)){
        
        ## Prints the message and returns the matrix from the cache
        message("getting cached data")
        return(m)
    }
    
    ## If the state above is false, then the inverse is calculated
    matx <- x$get()
    m <- solve(matx, ...)
    
    ## Now by the setmatrix function, the inversed matrix is put into cache
    x$setmatrix(m)
    
    ## prints the inversed matrix
    m
}