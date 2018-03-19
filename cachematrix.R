## These functions are a programming assigment 
## from coursera's course R Programming

## Usage:
##
## cachemtx <- makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)) )
## cacheSolve(cachemtx)

## This function creates a special "matrix" object that can cache its 
## inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  mtx<-NULL
  
  ## Set the value of te matrix, if the matrix is the same already setted, do nothing
  set <- function(y) {
    if(is.matrix(y))
    {
      if(dim(x) != dim(y) || !all(x == y))
      {
        x <<- y
        mtx <<- NULL
      }
      else
      {
        message("Matrix is the same")
      }
      
    }
       
  }
  ##Get the value of the orginal Matrix
  get <- function() x
  
  ##Set the inverse matrix
  setmtx <- function(matx) mtx <<-matx
  
  ##Get the value of the inverse of matrix
  getmtx <- function()mtx
  
  list(set = set, get = get,
       setmtx = setmtx,
       getmtx = getmtx)

}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmtx()
    
  if(!is.null(m) )
  {
      message("Getting Cached Matrix")
      return(m)
  }
  
  ## If the inverse wasn't solve already, solve and set it
  data <- x$get()
  m <- solve(data, ...)
  x$setmtx(m)
  m
  
}
