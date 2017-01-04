
## Put comments here that give an overall description of what your
## functions do
## function makeCacheMatrix  store a matrix 
# 1. set the value of the matrix, x
# x <-matrix(c(0:3),2,2)
# 2. get the value of the matrix
# misolve<-makeCacheMatrix(x)
#> misolve$get()
## variable x is assigned matrix whose inverse is to be found out
## cacheSolve calculates the inverse of a matrix 
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  x_i <- NULL
  set <- function(y) { ## for solving new matrix,y
    x <<- y            ## reassign new matrix, y to x
    x_i <<- NULL    ## reinitialize matrix inverse to null
  }
  get <- function() x
  setinverse <- function(mtrx_inverse) x_i <<- mtrx_inverse
  getinverse <- function() x_i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## Write a short comment describing this function
# 3. set the value of inverse of the matrix
# xi<- cacheSolve(misolve)
# 4. get the value of inverse of the matrix
# xi



cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mtrx_inverse <- x$getinverse()
  if(!is.null(mtrx_inverse)) {   ##if x is same matrix already solved, that is the last result
    message("getting cached data")
    return(mtrx_inverse)         ## returns last result
  }
  new_m <- x$get()                ## otherwise solve new matrix value
  mtrx_inverse <- solve(new_m, ...) ## calculates inverse
  x$setinverse(mtrx_inverse)      ## reassigns new matrix
  mtrx_inverse                     ## prints inverse
}


#> source("cachematrix.R")
#> x<-matrix(c(0:3),2,2)
#> misolve<-makeCacheMatrix(x)
#> misolve$get()
#[,1] [,2]
#[1,]    0    2
#[2,]    1    3
#> misolve$getinverse()
#NULL
#> cacheSolve(misolve)
#[,1] [,2]
#[1,] -1.5    1
#[2,]  0.5    0
#> misolve$getinverse()
#[,1] [,2]
#[1,] -1.5    1
#[2,]  0.5    0
