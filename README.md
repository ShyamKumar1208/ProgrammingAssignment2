###Introduction
Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

Write the following functions:

makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.

For this assignment, assume that the matrix supplied is always invertible.

R Code Implementation
<!-- -->
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  
  set <- function(y) {
  x <<- y
    inv <<- NULL 
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Get the cached inverse
  if (!is.null(inv)) {
    message("getting cached data")  # Message to indicate cached data is being used
    return(inv)  # Return the cached inverse
  }
  data <- x$get()  # Get the original matrix
  inv <- solve(data, ...)  # Calculate the inverse
  x$setInverse(inv)  # Cache the computed inverse
  inv  # Return the computed inverse
}

Create a special matrix object:

my_matrix <- matrix(c(4, 2, 3, 1), nrow = 2)
cached_matrix <- makeCacheMatrix(my_matrix)

Compute or retrieve the inverse:

inverse1 <- cacheSolve(cached_matrix)
print(inverse1)
inverse2 <- cacheSolve(cached_matrix)
print(inverse2)



