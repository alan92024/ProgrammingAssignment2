## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The makeCacheMatrix function is used to store data (a matrix x)
## and to create a new matrix (inv) to cache its inverse.  
## makeCacheMatrix returns a vector of four functions - 
## 'set' sets the value of the matrix, 'get' gets the matrix, 
## 'setinv' sets the value of the inverse of the matrix 
## (as calculated by cacheSolve), and 'getinv' gets the cached value
##  of the matrix if it exists, otherwise returns NULL.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## sets the matrix x equal to the matrix y, and deletes any
  ## existing cached data stored in inv
  set <- function(y){
    x <<- y
    inv <- NULL
  }
  
  ## returns the matrix x 
  get <- function() x
  
  ## sets the value of inv equal to the inverse of x as calculated 
  ## by the function cacheSolve   
  setinv <- function(inverse) inv <<- inverse
  
  ## returns NULL if inverse not yet calculated, or the cached
  ## value of inv if it has been calculated
  getinv <- function() inv
  
  ## the return value is a list of these four functions 
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function

## The function cacheSolve calculates the inverse of a matrix (x)
## passed to it if the inverse has not yet been calculated, or 
## returns the cached value if the inverse already exists.

cacheSolve <- function(x, ...) {
  
  ## get the value of the cached matrix from makeCacheMatrix - either
  ## NULL if it hasn't yet been calculated, or inv if it has been
  ## calculatd and stored 
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data"))
    return(inv)
  }
  
  ## get the data (matrix x) from makeCacheMatrix and assign it to
  ## the variable data
  data <- x$get()

  ## find the inverse of x using the solve function
  inv <- solve(x)

  ## set the value of the inverse of x stored in makeCacheMatrix to
  ## the calculated value
  x$setinv(inv)

  ## return the matrix inverse
  inv
}
