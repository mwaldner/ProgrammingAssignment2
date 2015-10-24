## The two functions below are used to create an object that stores a 
## matrix and caches its inverse

## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function (y){
    
      X <<- y
      inv <<- NULL 
      
  } 
  
  get <- function () x
  setinverse <- function(inverse)inv <<- inverse
  getinverse <- function () inv
  list( set = set,
        get = get,
        setinverse= setinverse,
        getinverse = getinverse
        )
}


## This function calculates the inverse of the matrix created by the function above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("Getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  
  inv
  
}
