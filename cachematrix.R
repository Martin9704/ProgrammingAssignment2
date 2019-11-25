## In order to avoid computing a matrix inversion we can cache it
## which is the target of my function.

## This short function creates a matrix object that can cache its inverse matrix

  makeCacheMatrix <- function(x = matrix()) {
    d <- NULL
    set <- function(y){
      x <<- y
      d <<- NULL
    }
    get <- function()x
    setInverse <- function(inverse) d <<- inverse
    getInverse <- function() d 
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
  }



## This function computes the inverse of the matrix created with makeCacheMatrix
  ## if that has already been done (and there are no chagnes to the matrix), then it
  # retrieves the inverse from cache

  cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    d <- x$getInverse()
    if(!is.null(d)){
      message("Getting cached data...")
      return(d)
    }
    mat <- x$get()
    d <- solve(mat,...)
    x$setInverse(d)
    d
  }
      

