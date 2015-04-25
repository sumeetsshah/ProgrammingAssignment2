## Author: Sumeet Sunil Shah
## Title: Programming Assignment 2
## Last Updated: April 15, 2015
## Notes: The functions below calculate the inverse of a matrix and cache the result so that
##        the calculation will not need to be repeated if the inverse is needed again in the
##        future. These functions assume that any matrices given as inputs are invertible
##        matrices.
##
## To Evaluators: I apologize for my heavy documentation. I like to have it so that if I return
##                to this code later, it will be easy to figure out what it does/how it works.
##                Hopefully it will help you understand my code better!


## The function makeCacheMatrix takes in an invertible matrix x.  The function caches
## the matrix as well as the matrix's inverse if the inverse has been calculated.
## This function contains get/set functions for both the original matrix and its inverse.
## Note that setting the originalMatrix to a new matrix using setMatrix(y) will set the 
## inverse to NULL, and while setting the inverse is allowed, it is not necessarily correct.
## This function does not return anything.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL                           ## Upon calling makeCacheMatrix, set the inverse to NULL, as it is currently unknown.
  getMatrix <- function() x                 ## Creates the getMatrix function, which takes no input and returns the cached matrix x.
  setMatrix <- function(y){                 ## Creates the setMatrix function, which takes in an invertible matrix y .
    x <<- y                                 ## Replaces the cached matrix x with the new matrix y.
    inverse <<- NULL                        ## Because the original matrix has been changed, set the inverse to NULL, as it is now unknown.
  }
  getInverse <- function() inverse          ## Creates the getInverse function, which takes no inputs and returns the cached inverse.
  setInverse <- function(i) inverse <<- i   ## Creates the setInverse function, which takes in a matrix and caches it as the inverse. 
  list(getMatrix = getMatrix,               ## Creates a list of the 4 get/set functions so that they can be referenced from outside
       setMatrix = setMatrix,
       getInverse = getInverse, 
       setInverse = setInverse)
}


## The cacheSolve function takes in the makeCacheMatrix and its arguments as input.
## If the cached matrix's inverse has already been calculated and stored, it retrieves
## the cached inverse and returns it. Otherwise it calculates the inverse, caches it, and
## returns it.
## This function returns the inverse of the cached matrix.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()                 ## Get the inverse of the cached matrix x
  if (!is.null(inverse)){                   ## If the inverse is not null
    message("Getting cached data")          ## Let the user know that cached data is being retrieved and
    return(inverse)                         ## Return the inverse
  }
  originalMatrix <- x$getMatrix()           ## Else retrieve the original matrix
  inverse <- solve(originalMatrix, ...)     ## Solve for the original matrix x's inverse
  x$setInverse(inverse)                     ## Cache the calculated inverse value
  return(inverse)                           ## Return the inverse of the original matrix x
}
