## These functions allow computation of a special object
## that can cache its inverse

## This function generates a special matrix that
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      j <- NULL
      set <- function(y){
            x <<- y
            j <<- NULL
      }
      get <- function()x
      setInverse <- function(inverse) j <<- inverse
      getInverse <- function() j 
      list(set = set, get = get, 
           setInverse = setInverse, 
           getInverse = getInverse)
}


## This function calculates the inverse of the special matrix
## generated above

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      j <- x$getInverse()
      if(!is.null(j)){
            message("getting cached data")
            return(j)
      }
      mat <- x$get()
      j <- solve(mat,...)
      x$setInverse(j)
      j
}