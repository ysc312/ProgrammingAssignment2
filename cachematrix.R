## This function creates a special matrix that will cache its own inverse
## This function contain a list of "set", "get", "setinverse", "getinverse"
makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y) {
    X<<-y
    i<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse)i<-inverse
  getinverse<-function()i
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)                            
}
## This function computes the inverse of the special "matrix" created by the "makeCacheMatrix function above. 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i<-x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  matrix<-x$get()
  i<-solve(matrix,...)
  x$setinverse(i)
  i
  
}

