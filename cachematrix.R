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
    solve(m)  
  else{
    exp.mat(m, -1)
  } 
}

#This code is from here: http://stackoverflow.com/questions/21364060/calculate-inverse-of-a-non-square-matrix-in-r
#The exp.mat function performs can calculate the pseudoinverse of a matrix (EXP=-1)
#and other exponents of matrices, such as square roots (EXP=0.5) or square root of 
#its inverse (EXP=-0.5). 
#The function arguments are a matrix (MAT), an exponent (EXP), and a tolerance
#level for non-zero singular values.
exp.mat<-function(MAT, EXP, tol=NULL){
  MAT <- as.matrix(MAT)
  matdim <- dim(MAT)
  if(is.null(tol)){
    tol=min(1e-7, .Machine$double.eps*max(matdim)*max(MAT))
  }
  if(matdim[1]>=matdim[2]){ 
    svd1 <- svd(MAT)
    keep <- which(svd1$d > tol)
    res <- t(svd1$u[,keep]%*%diag(svd1$d[keep]^EXP, nrow=length(keep))%*%t(svd1$v[,keep]))
  }
  if(matdim[1]<matdim[2]){ 
    svd1 <- svd(t(MAT))
    keep <- which(svd1$d > tol)
    res <- svd1$u[,keep]%*%diag(svd1$d[keep]^EXP, nrow=length(keep))%*%t(svd1$v[,keep])
  }
  return(res)
}
