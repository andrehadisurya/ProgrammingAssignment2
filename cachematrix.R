makeCacheMatrix <- function(x=matrix()){
  i <- NULL
  set <- function(y){
      x <<- y
      i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}
cacheSolve <- function(x, ...){
  i <- x$getInverse()
  if (!is.null(i)){
      message ("Getting Cache Data")
      return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$getInverse(i)
  i
}
