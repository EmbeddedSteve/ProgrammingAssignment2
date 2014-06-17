## Coursera R Programming Assignment 2
## Author:  embeddedsteve@gmail.com
## Leixcal scoping of R and creating a "list of functions" function
## Includes concept of closure and engages scoping rules

#
# This function provides the 4 support functions:
# 1.  set() - sets the value of the matrix
# 2.  get() - gets the value of the matrix
# 3.  setInverse() - this is where the inverse gets "cached"
# 4.  getInverse() - retrieves the "cached" data
#

makeCacheMatrix <- function(x = matrix()) {
  
  invMatrix <- NULL
  
  # set()
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  
  # get() - inline
  get <- function() x
  
  # setInverse() - 
  setInverse <- function(newMatrix) invMatrix <<- newMatrix
  
  # getInverse()
  getInverse <- function() invMatrix
  
  # Here we return these functions defined above
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}    

#
# This is the top level function that will
# utilize the 4 helper functions defined above. 
# This checks for the cached data to be returned
#

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  cachedInverse <- x$getInverse()
  if( !is.null(cachedInverse) ){
    message("getting cached data")
    return( cachedInverse )
  }
  
  # set the inverse, using the raw matrix data
  message("nothing cached - compute inverse")
  data <- x$get()
  invData <- solve(data)
  x$setInverse(invData)
  return( invData )
}



