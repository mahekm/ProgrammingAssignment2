## The following two functions cache the value of the inverse of a matrix.
## Computing the inverse of a large matrix is a computationally expensive process.
## Hence,caching or temporary storage helps in saving time and memory.


## The following function creates the matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse1 <- NULL
  set <- function(y) {
    x <<- y
    inverse1 <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inverse1 <<- inverse
  getinv <- function() inverse1
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The function calculates the inverse of the matrix object created by 
## makeCacheMatrix. If the value has already been calculated and the 
## matrix doesnt change, it returns the value from the cache.
cacheSolve <- function(x, ...) {
  inverse1 <- x$getinv()
  if(!is.null(inverse1)) {
    message("getting cached data")
    return(inverse1)
  }
  data <- x$get()
  inverse1 <- solve(data, ...)
  x$setinv(inverse1)
  inverse1
}


