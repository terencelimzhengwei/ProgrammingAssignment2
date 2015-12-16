## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  get <- function() x
  setInverse<-function(inverse) inv<<-inverse
  getInverse<-function() inv
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## This function computers the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has been
## calculated (and the matrix not changed), it will return the mean
## from its cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached inverse data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setInverse(m)
  m
}
