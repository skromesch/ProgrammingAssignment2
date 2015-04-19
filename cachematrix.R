## Handling a matrix and its inverse using a cache mechanism


## Create a list with 4 functions handling a matrix and its inverse
## functions:
##    set(x)
##    get():x
##    setinverse(inv)
##    getinverse()

makeCacheMatrix <- function(x = matrix()) {
  # variable: store matix inverse
  inverse <- NULL
  # function: store the matrix
  set <- function(y) {
    x <<- y
    # set inverse matrix to NULL cache is empty
    inverse <<- NULL
  }
  # function: get the stored matrix
  get <- function() x
  # function set inverse matrix
  setinverse <- function(inv) inverse <<- inv
  # function: get inverse matrix
  getinverse <- function() inverse
  # return: a list with the 4 function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Get the inverse of a matrix stored in a List created by makeCaheMatrix function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  # if cache is not empty retrun cached data
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # cache empty load data to cache and return data
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
