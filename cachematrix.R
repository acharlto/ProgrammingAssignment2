## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  #this line is not used in cachesolve but it could be used to change the matrix
  #"x" given in makeCacheMatrix to a matrix "y".  this would require introducing 
  #a second variable "y" in the cachesolve function
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  invx<<-solve(x)
  #this line will eventually get the matrix "x"
  get <- function() x
  #the "solve" below is a variable and not a function
  #this line stores "solve" in cache as "m"
  setinv <- function(solve) m <<- solve
  #return m
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

#the variable x is the "special matrix" created with 
#makeCacheMatrix.  ex.  SpecMat<-makeCacheMatrix(matrix(c(1,2,3,4),nrow=2,ncol=2))
#SpecMat would be the input x
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #pulls in the solve function but before it calculates it tests 
  #to see if m has a value in cache.  if it does it returns m 
  #without repeating the calculation and ends the function
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #if no data is stored in cache the function continues
  #this line gets the matrix x from the "makeCacheMatrix function
  data <- x$get()
  #get inverse of "data" and store as m
  m <- solve(data)
  #this line stores the variable m in cache
  x$setinv(m)
  #return m
  m
}