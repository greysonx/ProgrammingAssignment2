## The first function creates a "matrix", which is a list containing a function to
## 1.Set the value of the matrix
## 2.Get the value of the matrix
## 3.Set the value of the inverse matrix
## 4.Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function() inver
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function is going to calculate the inverse matrix.
## The first thing is to check whether the inverse matrix has already been calculated.
## If so, then the cacheSolve should retrieve the inverse matrix from the cache.
## Otherwise, inverse matrix should be calculated and we set the value of the inverse matrix through setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inver <- x$getinverse()
  if (!is.null(inver)) {
    message("getting cached inverse")
    return(inver)
  }
  matri <- x$get()
  inver <- solve(matri, ...)
  x$setinverse(inver)
  inver
}
