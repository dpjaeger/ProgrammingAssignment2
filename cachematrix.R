## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  n<-NULL
  set<-function(y){
    x<<-y
    n<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) n<<- solve
  getmatrix<-function() n
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}
## This function creates a matrix object that can cache its inverse 
## set a matrix, get the matrix, set the inverse matrix, and get the inverse matrix

cacheSolve <- function(x, ...) {
  n<-x$getmatrix()
  if(!is.null(n)){
    message("Getting Cached Data")
    return(n)
  }
  matrix <- x$get()
  n<-solve(matrix, ...)
  x$setmatrix(n)
  n
}
## This returns a matrix that is the inverse of x.
## If this result exists in the cached data, it uses that matrix
