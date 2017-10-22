##These functions deal with a created cacheMatrix object
##which can store a matrix as well as its inverse


##makeCacheMatrix creates a list containing 4 functions
##which can be called to set or retrieve data about a matrix
##and its inverse

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  getm <- function() x
  setm <- function(y){
    x <<- y
    inv <<-NULL
  }
  getinv <- function() inv
  setinv <- function(i) inv <<- i
  
  list(setm = setm, getm=getm, getinv = getinv, setinv = setinv)
}

##cacheSolve checks to see if the cacheMatrix has already been solved.
##If it has been, the function returns the solution, otherwise
##it calculates the solution and stores it in the object
cacheSolve <- function(x,...){
  
  i <- x$getinv()
  if(!is.null(i)){
      message('getting cached data')
      return(i)
  }
  data <- x$getm()
  i <- solve(data)
  x$setinv(i)
  i
}