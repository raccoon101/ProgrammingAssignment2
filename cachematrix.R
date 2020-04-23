## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {## define the argument with default mode of "matrix"
  j <- NULL                                ## initialize j as NULL; will hold value of matrix inverse
  set <- function(y){                      ## define the set function to assign new 
    x <<- y                                ## value of matrix in parent environment
    j <<- NULL                             ## if there is a new matrix, reset j to NULL
  }
  get <- function()x                       ## define the get fucntion - returns value of the matrix argument  
  setInverse <- function(inverse) j <<- inverse  ## assigns value of inv in parent environment
  getInverse <- function() j                     ## gets the value of j where called
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)           ## you need this in order to refer 
                                          ## to the functions with the $ operator
}





## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}
## The function cacheSolve takes the output of the previous matrix makeCacheMatrix(matrix) as an 
# input and checks inverse matrix from makeCacheMatrix(matrix) has any value in it or not.
# In case inverse matrix from makeCacheMatrix((matrix) is empty, it gets the original matrix data from 
# and set the invertible  matrix by using the solve function.
# In case inverse matrix from makeCacheMatrix((matrix) has some value in it (always works
#after running the code 1st time), it returns a message  "Getting Cached Invertible Matrix" 
#and the cached object

