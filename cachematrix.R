## Functions provided are :
## makeCacheMatrix : matrix cache
## cacheSolve : set and update matrix cache, use makeCacheMatrix


## Make a object which :
## set a matrix
## get a matrix
## set inverse of a matrix
## get inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Return inverse of a matrix
## Get a makeCacheMatrix as argument (use makeCacheMatrix as a cache to compute inverse matrix only once)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
