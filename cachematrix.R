

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  ##sets the vector
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x  ##gets the vector
  setinverse <- function(inverse) i <<- inverse  ####sets the value of inverse
  getinverse <- function() i  ##gets the vslue of inverse
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}
##if the cache is empty(ie if the inverse is yet to be calculated),
##it will first compute the value and then store it in the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ##remember the matrix value to be set should be invertible when 
  ##when you test this code.otherwise,error would occur
 
  message("first time computation")
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
