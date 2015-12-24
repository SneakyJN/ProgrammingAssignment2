## This file contains two functions:
##  - makeCacheMatrix
##  - cacheSolve
##
## The two functions are related and makeCacheMatrix should be called before cacheSolve
## NOTE: makeCacheMatrix assumes that an invertible/square matrix is always supplied


## FUNCTION: makeCacheMatrix 
##  parameter 1: an invertible/square matrix
##  return: returns a list with 4 functions:
##        Function 1: "set" - should not be used.  Allows the user to manually set the inverse of the matrix
##        Function 2: "get" - simply returns the matrix that isused for the input to makeCacheMatrix
##        Function 3: "setSolve" - stores the inverse of the matrix into the cache variable "m"
##        Function 4: "getSolve" - gets the value currently stored in the cache variable "m"
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setSolve <- function(theSolve) m <<- theSolve
  
  getSolve <- function() m
  
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## FUNCTION: cacheSolve
##  parameter 1: a list of 4 functions created by using the makeCacheMatrix function above
##  return: returns a matrix of numberals that is the inverse of the matrix supplied to the makeCacheMatrix fuction
##
## NOTE: This function will first check the cache variable "m" to see if the inverse has already been computed, 
##  and is stored in the environment.  If the cache is empty/blank, the inverse is calculated and then stored back 
##  into the "m" cache variable
##
cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  
  if(!is.null(m)) {
    message("getting cached data for the Matrix Solve")
    return(m)
  }
  
  #ELSE, if m is null
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}