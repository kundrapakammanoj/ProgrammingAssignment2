
# makeCacheMatrix function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setMatrix<-function(solve) m<<-solve
  getMatrix<-function() m
  list(set=set,get=get,
       setMatrix=setMatrix,
       getMatrix=getMatrix)
  
}


##cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix function.

cacheSolve <- function(x, ...) {
  m <- x$getMatrix()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setMatrix(m)
  m 
}
