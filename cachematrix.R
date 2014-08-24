## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix creates a list containing following functions

# setMatrix(matrix): used to set the matrix

# getMatrix(): return the matrix

# setInverseMatrix(inversMatrix): used to inverse of the matrix

# getInverseMatrix(): inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  
  setMatrix <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  
  getMatrix <- function(){
    x  
  } 
  
  setInverseMatrix <- function(inverse) { 
    inverse_matrix <<- inverse
  }
  
  getInverseMatrix <- function() { 
    inverse_matrix
  }
  #create list of functions
  list(setMatrix=setMatrix, getMatrix=getMatrix, setInverseMatrix=setInverseMatrix, getInverseMatrix=getInverseMatrix)
}


# This function first checks if the inverse of Matric exists, 
# If "Yes" : Then returns the cached values of inverse of matrix
# If "No" : 

# Assumptions:  matrix is always invertible

cacheSolve <- function(x, ...) {
  
  inverse_matrix <- x$getInverse()

  if(!is.null(inverse_matrix)) {
    message("Inverted Matric exists already ! Returing the cached value ")
    return(inverse_matrix)
  }
  
  # if inverse matrix cache does not exists , this block will calculate the same and return
  data <- x$getMatrix()
  inverse_matrix <- solve(data)
  x$setInverseMatrix(inverse_matrix)
  
  #return inverse
  inverse_matrix
} 