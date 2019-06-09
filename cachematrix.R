## Put comments here that give an overall description of what your
## functions do
## the Matrix inversion is usally a costly computation, so we would want to cache
## it inverse
## This pair of functions will cachce the inverse of a matrix 
## and also slove the inverse of a matrix if not cached.
## Write a short comment describing this function

# Returns a list of four fucntions that get and sets the value of the matrix.
# And then to set and get the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  ## define the function for setting the value of matrix and clear old cache if exist
  i <- NULL
  set <- function(y) {
    ## <<- is used for caching
    x <<- y
    i <<- null
  }
  ## Define function to get the value of a matrix
  get <- function() x
       ## setinverse is only used when thier is no cache data.
  setinverse <- function(inverse) i <<-- inverse
       # define the function for getting the inverse
  getinverse <- function() i 
  ## Returns  list of he defined function above
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## Short Comment:
## Author: Justino Garcia
## using the function above and checking if data is cached or not
## And solves the inverese matrix of a 2 by 2 (Square)matrix, using new data 
## Or cached data.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    #since i is not null, it will go and grab the cached data
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <-solve(data, ...)
  x$setinverse(i)
  i # Returns the the inverse matrix 
}

#Test

x <- matrix(c(1,2,3,4),2,2)

x1 <- makeCacheMatrix(x)

cacheSolve(x1)


# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
