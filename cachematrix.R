## These functions will allow us to cache and retreive
## the inversion of a matrix


## makeCacheMatrix creates our matrix object and will
# store the cached inversion matrix.  It contains getters
# and setters for the matrix object

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv)

}


## cacheSolve will first determine if the cached inverted 
## matrix exists in the matrix object. 
##If it does not exist, it will calculate the inverse 
## and cache it inside the matrix object. 
##If it does exist it will 
## retreive it from the matrix object and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
