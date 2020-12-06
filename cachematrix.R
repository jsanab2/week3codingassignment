## This function creates a matrix and a function to set the inverse of such
## matrix and to get inverse of the such matrix

## Write a short comment describing this function
## This function first creates a matrix "x" then creates a vector for its
## inverse, and a function to set and to get such inverse

makeCacheMatrix <- function(x = matrix()) {
    cacheinv <- NULL
    set <- function(y) {
      x <<- y
      cacheinv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) cacheinv <<- inverse
    getinverse <- function() invrs
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function solves a matrix, if it has been solved then it returns the
## stored value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    cacheinv <- x$getinverse()
    if(!is.null(cacheinv)) {
      message("getting cached data")
      return(cacheinv)
    }
    matdata <- x$get()
    cacheinv <- solve(matdata, ...)
    x$setinverse(cacheinv)
    cacheinv
}
