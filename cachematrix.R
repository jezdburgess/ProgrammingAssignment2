
## The functions below generate a matrix and calculate its inverse and if it
## has not already been carried out, calculates its inverse. If the inverse has
## been previously calculated and the matrix has not changed, the inverse is
## retrieved from the cache to save processing time. 

## makeCacheMatrix generates a special 'matrix' object,which is really a list
## containing functions to:
## - set the values of the matrix
## - get the values of the matrix
## - set the values of the inverse of the matrix
## - get the values of the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
  
  mat <- NULL
  set <- function (y) {
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  setinv <- function(solve) mat <<- solve
  getinv <- function() mat
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}

## cacheSolve calculates the inverse of the matrix generated in the function above.
## If the inverse has already been calculated and the matrix has not changed, the inverse
## is retrieved from the cache rather than getting recalculated. Otherwise, the inverse is
##calculated and stored in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    mat <- x$getinv()
    if(!is.null(mat)) {
      message("getting cached data...")
      return(mat)
    }
    data <- x$get()
    mat <- solve(data, ...)
    x$setinv(mat)
    mat
  }

## Example usage: 
## InputMatrix <- matrix(data = rnorm(25), nrow = 5, ncol = 5) # Generate random 5x5 matrix
## makeCacheOutput <- makeCacheMatrix(InputMatrix)
## cacheSolve(makeCacheOutput) # if run twice, second should be quicker and print 'getting cached data...' message.

  