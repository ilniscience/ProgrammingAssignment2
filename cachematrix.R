## Put comments here that give an overall description of what your
## functions do
##
## This code is a pair of functions that caches the inverse of a matrix.
##
## Write a short comment describing this function:
##
## makeCacheMatrix function creates a special "matrix" object
## that can cache its inverse
##
## The code is similar to makeVector function given in the
## Programming Assignment 2 instructions
##
## The function returns a list of four functions that will be used to:
## set and get matrix data
## set and get the inverse of matrix to/from the cache
##
makeCacheMatrix <- function(x = matrix()) {
  # 
  inversematrix <- NULL
  set <- function(y) { # to set matrix data and initialize the cache
    x <<- y
    inversematrix <<- NULL
  }
  get <- function() x # to get matrix data
  setinverse <- function(inverse) inversematrix <<- inverse # to set the inverse to the cache
  getinverse <- function() inversematrix # to get the inverse from the cache
  
  # return the list of the above functions
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  

}


## Write a short comment describing this function
##
## cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix function given above.
## It uses the list of functions from makeCacheMatrix.
##
## The code is similar to cachemean function given in the
## Programming Assignment 2 instructions.
##
## If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

## Otherwise, computes the inverse of a square matrix  
## using solve function in R.
## We assume that the matrix supplied is always invertible.
## 


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ##
  inversematrix <- x$getinverse() # retrieve the inverse from the cache
  if(!is.null(inversematrix)) {   # check if it's already computed
    message("return the inverse retrieved  from the cache")
    return(inversematrix)
    
  }
  matrixdata <- x$get()               # otherwise, get the matrix data
  inversematrix <- solve(matrixdata)  # compute the inverse
  x$setinverse(inversematrix)         # cache the inverse
  
  inversematrix                       # return the inverse
  
}
