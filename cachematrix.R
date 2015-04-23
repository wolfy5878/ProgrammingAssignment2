## To save time and build efficiencies in functions, such as calculating inverse
## of a matrix, we're building a cache of the initally run inverse matrix to pull
## from on subsequent runs.

## This first function creates a special object that can cache the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
     s <- NULL
     set <- function(y) {
          x <<- y
          s <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) s <<- solve
     getinverse <- function() s
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## The below function will calculate the inverse matrix or pull it
## from cache, if it was previously run and all matrix data hasn't changed

cacheSolve <- function(x, ...) {
     s <- x$getinverse()
     if(!is.null(s)) {
          message("getting cached data")
          return(s)
     }
     data <- x$get()
     s <- solve(data, ...)
     x$setinverse(s)
     s
}
