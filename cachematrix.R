## This file contains functions for calculating inverse matrix using cache

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(matrix) {
  setmatrix <- function(m) {
    if (!is.matrix(m)){
      # User should be informed when he does something wrong
      warning('It isn\'t martix!')
      matrix <<-NULL
    }else
      matrix <<- m
    inverse <<- NULL
  }
  getmatrix <- function() matrix
  getinverse <- function() inverse
  setinverse <- function(inv) inverse <<- inv
  setmatrix(matrix)
  list(set = setmatrix, get = getmatrix,
       getinverse = getinverse,
       setinverse = setinverse)  
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix function.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(cacheMatrix) {
  ## Return a matrix that is the inverse of 'x'
  inv <- cacheMatrix$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- cacheMatrix$get()
  if(is.null(data)) {
    warning("Matrix isn't set")
    return(NULL)
  }
  inv <- computeInverse(data)
  cacheMatrix$setinverse(inv)
  inv
}

## This function calculates inverse for matix m
computeInverse <- function(m){
  if (!is.matrix(m)){
    # User should be informed when he does something wrong
    warning('It isn\'t martix!')
    return(NULL)
  }
  if (dim(m)[1] == dim(m)[2])
    return(solve(m))  
  else{
    warning('Sorry, but i can work only with square matrix :( ')
    return(NULL)
  } 
}
