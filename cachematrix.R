## the 'makecachematrix' function makes an object
## that can cache the result of inversing that matrix


makeCacheMatrix <- function(x = matrix()) {

m <- NULL
  set <- function(y){
    x <<- y
  m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## checks to see if the solution of inversing a matrix has
## already been cached, if it has, it will return the solution
## if not, it will solve for the inverse of the matrix

cacheSolve <- function(x, ...) {
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
