## Function makeCacheMatrix creates a list that sets the value of a matrix,
## gets the value of the matrix, sets the inverse of that matrix, and gets
## the value of the inverse. 

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
      x <<- y
      i <<- NULL
      }
      get <- function() x
      setinv <- function(solve) i <<- solve
      getinv <- function() i
      list(set = set, get = get,
      setinv = setinv,
      getinv = getinv)
}



##Function cacheSolve calculates the inverse of the matrix X created with the 
## previous function. It its already been calculated, it just returns the
## the inverse of the matrix. ## If it has not been calculated it calculates 
## the inverse of the matrix. 
 

cacheSolve <- function(x, ...) {
      i <- x$getinv()
      if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
  }     

