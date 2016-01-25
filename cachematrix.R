## Custom function to cache the inverse of a matrix
## Coursera - R Programming - Week 3


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # Function to set a matrix to an object created by the makeCacheMatrix function
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x # returns the input matrix
  setInv <- function(inv) m <<- inv # set the inversed matrix
  getInv <- function() m # return the inversed matrix
  
  # create a list containing the functions above for later use.
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


# Computes the inverse of the matrix returned by makeCacheMatrix.
# The inverse will be retrieved from the cache if the inverse was already calculated and not changed.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInv(m)
  m
}
