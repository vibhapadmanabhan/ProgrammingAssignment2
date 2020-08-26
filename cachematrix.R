## Computing the inverse of a matrix repeatedly is a costly operation, so it makes
## sense to store the inverse in a cache so that it can be looked up again if
## needed. The functions below create a 'matrix' object that can cache its inverse
## and compute/retrieve the inverse of the matrix.

## makeCacheMatrix creates a special 'matrix' object that can cache its inverse.
## The 'matrix' object is actually a list containing a function to set the value
## of the matrix, get the value of the matrix, set the value of the inverse
## and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  get<-function() x
  setinverse<-function(solve) inverse<<-solve
  getinverse<-function() inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}

## cacheSolve computes the inverse of the special 'matrix' returned by 
## makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve retrieves the 
## inverse from the cache. Otherwise, it calculates the inverse and sets the value
## of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  inverse<-x$getinverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data<-x$get()
  inverse<-solve(data)
  x$setinverse(inverse)
  return(inverse)
}
