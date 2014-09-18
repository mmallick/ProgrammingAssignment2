##
## This R program contain two functions makeCacheMatrix() and cacheSolve(). 
##
## - makeCacheMatrix() creates a special "matrix" object which is really list
## of getter and setter for original matrix and its inverse matrix
##
## - cacheSolve() computes inverse of matrix by retrieving matrix 
## from the object created through makeCacheMatrix() function and  
## stores it back on the same object. 
## 
## > source("ProgrammingAssignment2/cachematrix.R")
# > special_matrix <- makeCacheMatrix(matrix(rnorm(1:9), 3,3))
# > special_matrix$get()
# [,1]       [,2]       [,3]
# [1,] -1.2281899  0.5180830  1.6553127
# [2,]  1.3252701 -0.1353303  0.6557099
# [3,]  0.9640201  0.3490221 -0.3648536
# > cacheSolve(special_matrix)
# [,1]       [,2]       [,3]
# [1,] -0.1008294  0.4307545  0.3166909
# [2,]  0.6267498 -0.6447258  1.6848235
# [3,]  0.3331419  0.5213938 -0.2923447
# > special_matrix$get() %*% special_matrix$getinverse()
# [,1]          [,2]         [,3]
# [1,]    1 -1.110223e-16 0.000000e+00
# [2,]    0  1.000000e+00 2.775558e-17
# [3,]    0 -2.775558e-17 1.000000e+00
# > cacheSolve(special_matrix)
# getting from cache
# ...

## Description: Creates a object which stores both orginal matrix 'x' 
## and its inverse 'inv_x'. This object contains list of functions 
## which are used to set and get these matrixes.
makeCacheMatrix <- function(x = matrix()) {
  
  inv_x <- NULL
  
  ## setter function for matrix
  set <- function(y) {
    ## assigning the matrix
    x <<- y
    ## reseting the inverse of matrix to null because matrix has changed
    inv_x <<- NULL
  }
  
  ## getter function for matrix
  get <- function() x
  
  ## setter function for inverse matrix
  setinverse <- function(inverse) inv_x <<- inverse
  
  ## getter function for inverse matrix
  getinverse <- function() inv_x
  
  ## return list of all functions for function makeCacheMatrix()
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## Description: It return a matrix that is the inverse of matrix from object. 
## It only creates inverse for the first time and stores it on the object.
## To create inverse matrix, input matrix need to a square matrix.
cacheSolve <- function(x, ...) {
  
  inv_x <- x$getinverse()
  
  ## checking for existence of inverse matrix
  if(!is.null(inv_x)) {
    message("getting from cache")
    return(inv_x)
  }
  
  ## checking for square matrix otherwise erroring out
  y <- x$get()
  if (ncol(y) != nrow(y)) {
    message("Error: matrix is not a square matrix")
    return(NULL)
  }
  
  inv_x <- solve(y, ...)
  x$setinverse(inv_x)
  
  inv_x
}
