## makeCacheMatrix returns a list, including results of 4 functions
##'set', 'get', 'setInv', 'getInv'

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    m <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inv){
    inv <<- solve(x) ## matrix supplied is always invertible
  }
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve returns inverse matrix 'x' from the cache memory

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(x)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <<- solve(data)
  x$setInv(inv)
  inv ## Return a matrix that is the inverse of 'x'
}
