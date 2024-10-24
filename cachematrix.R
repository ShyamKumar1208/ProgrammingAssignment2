# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the inverse cache when the matrix is set
  }
  
  # Function to get the matrix
  get <- function() x
  
  ## Function to set the cached inverse
  setInverse <- function(inverse) inv <<- inverse
  
  ## Function to get the cached inverse
  getInverse <- function() inv
  
  ## Return a list of the functions to access the matrix and its inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function to compute the inverse of the special matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Get the cached inverse
  
  # Check if the inverse is already cached
  if (!is.null(inv)) {
    message("getting cached data")  # Message to indicate cached data is being used
    return(inv)  # Return the cached inverse
  }
  
  # If not cached, compute the inverse
  data <- x$get()  # Get the original matrix
  inv <- solve(data, ...)  # Calculate the inverse
  x$setInverse(inv)  # Cache the computed inverse
  inv  # Return the computed inverse
}
