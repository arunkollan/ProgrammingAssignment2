## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##The first function, makeVector creates a special "matrix", which is really a list containing a function to
##set the value of the vector
##get the value of the vector
##set the value of the mean
##get the value of the mean
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## Write a short comment describing this function
## The function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  matrix <- x$get() 
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
