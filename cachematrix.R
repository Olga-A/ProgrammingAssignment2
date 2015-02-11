## Put comments here that give an overall description of what your
## functions do

## Is used for caching matrix. 
## Creates a special matrix with assigning "x" to a variable 
## in the other environment. 

makeCacheMatrix <- function(x = matrix()) {

  cX <- NULL
  get <- function() x
  setImatrix <- function(Imatrix) 
  cX <<- Imatrix
  getImatrix <- function() cX
  
  # return a list of functions as an R object
  list(get=get, 
       setImatrix=setImatrix, 
       getImatrix=getImatrix)
}


##Return a matrix that is the inverse of "x"

cacheSolve <- function(x, ...) {
        
  cX <- x$getImatrix()
  if(!is.null(cX)){
    ## message if chached data exists
    message("Cached data found... Result:")
    return(cX)
  }
  else {
    ## message if there is no chached data
    message("No cached data found. Calculating...")
    data <- x$get() # obtains matrix from object x
    cX <- solve(data) # finds inverse matrix
    x$setImatrix(cX) # assigns resulting inverse matrix to object x
    message("Inverse matrix:")
    return(cX)
  }}
