## makeCacheMatrix calculates the inverse of the matrix and caches the result.
## cacheSolve calls the cached inverse, or, if it is missing, will calculate the inverse
##of the matrix.

##  makeCacheMatrix calculates the inverse of the matrix and caches the result.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set<-function(y){
    x<<-y
    m<<- NULL
  }
  get<-function() x
  setinverse<-function(solve) m<<- solve
  getinverse<-function() m
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## cacheSolve calls the cached inverse, or, if it is missing, will calculate the inverse
##of the matrix. I found chapter 7 in 'R For Dummies' helpful in these sections.

cacheSolve <- function(x, ...) {
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setinverse(m)
}
