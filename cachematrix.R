## Matrix inversion is usually a costly computation
## and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly. 
## Below are pair of functions that cache the inverse of a matrix.


## This function, makeCacheMatrix creates a special matrix object , 
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  
  set <- function(y) {
    
    x <<- y
    
    invs <<- NULL
  }  
    get <- function() x
    
    setinverse <- function(inverse) invs <<- inverse
    
    getinverse <- function() invs
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
    
}




## This function cacheSolve computes 
## the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## then it skips the computation and retrives the inverse from the cache else
## it computes the inverse, sets the value in the cache via
## setinverse function.

cacheSolve <- function(x, ...) {
  invs <- x$getinverse()
  
  if(!is.null(invs)) {
    
    message("getting cached data")
    
    return(invs)
    
  }
  
  data <- x$get()
  
  invs <- solve(data)
  
  x$setinverse(invs)
  
  invs
}
