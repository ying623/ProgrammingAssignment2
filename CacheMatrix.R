##Matrix inversion is usually a costly computation and there may be some benefit
##to caching the inverse of a matrix rather than compute it repeatedly 
##the following functions that cache the inverse of a matrix.

##creates a CacheMatrix object that can cache its inverse
##The first function, makeCacheMatrix creates a special "CacheMatrix", which is really a list containing a function to
##1.set the value of the matrix
##2.get the value of the matrix
##3.set the value of the inverse of matrix
##4.get the value of the inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
          x <<- y
          inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse) 
}

## This function computes the inverse of the "CacheMatrix" object created by 
## makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
          message("getting cached data")
          return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

##> Sample run:
##> sample 1:
##> x=matrix(1:4,2,2)
##> my=makeCacheMatrix(x)
##> my$get()
##    [,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> my$getInverse()
##NULL
##> cacheSolve(my)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(my)
##getting cached data
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> my$getInverse()
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

##> sample 2:
##> x2=matrix(c(6,6,7,8),2,2)
##> my2=makeCacheMatrix(x2)
##> my2$get()
##     [,1] [,2]
##[1,]    6    7
##[2,]    6    8
##> my2$getInverse()
##NULL
##> cacheSolve(my2)
##          [,1]      [,2]
##[1,]  1.333333 -1.166667
##[2,] -1.000000  1.000000
##> cacheSolve(my2)
##getting cached data
##          [,1]      [,2]
##[1,]  1.333333 -1.166667
##[2,] -1.000000  1.000000
##> my2$getInverse()
##          [,1]      [,2]
##[1,]  1.333333 -1.166667
##[2,] -1.000000  1.000000
