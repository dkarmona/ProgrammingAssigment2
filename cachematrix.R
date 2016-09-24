## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     # This function creates a special "matrix" object that can cache its inverse
     inversa<-NULL
     set<-function(y){
          x<<-y
          inversa<<-NULL
     }
     get<-function() x
     setinversa<-function(inverse) inversa<<-inverse
     getinversa<-function() inversa
     list(set=set,get=get,setinversa=setinversa,getinversa=getinversa)
}

## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated (and 
# the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
     inversa<-x$getinversa()
     if(!is.null(inversa)){
          message("getting cached data")
          return(inversa)
     }
     datos<-x$get()
     inversa<-solve(datos,...)
     x$setinversa(inversa)
     inversa
}
