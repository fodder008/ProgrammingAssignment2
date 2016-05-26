## Makes a matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function () x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function attempts retrieve the inverse of the matrix from the cache
## If the inverse has not been calculated yet then it is calculated

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  mtrx <- x$get()
  i <- solve(mtrx,...)
  x$setinverse(i)
  i
}
