makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { #set the value of the vector
    x <<- y
    m <<- NULL
  }
  get <- function() x # get the value of the vector
  
  setinverse <- function(inverse) m <<- inverse # set the value of the inverse matrix
  getinverse <- function() m # get the value of the inverse matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x=matrix, ...) {
  
  m <- x$getinverse() 
  
  if(!is.null(m)) { # define whether "m" is not null (already calculated)
    message("getting cached data")
    return(m) # since "m" exist in the working eviromnent it return the inversed matrix with calculation needed
  }
  data <- x$get() # "m" does not exist so the function called the underlying matrix
  m <- solve(data, ...)
  x$setinverse(m) # the inverse matrix is then calculated 
  m
}

