## makeCacheMatrix initializes a matrix variable, and establishes a few 
## functions (effectively methods) for cacheSolve to work with.



## m will store the inverse once it is calculated. It is initialized as 
## NULL everytime x (the matrix) is initialized or changed so that
## cacheSolve can decide whether the inverse has to be calculated or
## if it can just be recalled. get() grabs the value of the matrix,
## setinverse() caches the inverse so that cacheSolve can just recall
## the value if it's already calculated, getinverse() grabs the value of
## m whether it is calculated as the inverse yet, or not. List provides
## a list of the methods we have just defined.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## First, this function grabs the value of m, and then tests to see if it is
## an inverse value or if it is still set to NULL. If it is an inverse value,
## it returns it, but if it's NULL, it grabs the value of the matrix, stores
## the calculation of its inverse locally, and then uses setinverse() to
## cache the data in the parent environment. Finally, it returns the value of
## m, which will be the inverse, whether it is recalled or calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
