## These functions work together to cache the inverse of a matrix to avoid
## costly repeated computations. makeCacheMatrix creates a special matrix object
## that can store both the original matrix and its cached inverse, while
## cacheSolve computes the inverse only if it hasn't been calculated before.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It returns a list of functions to:
## 1. set the matrix
## 2. get the matrix  
## 3. set the inverse
## 4. get the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y      # Set the matrix in the parent environment
    inv <<- NULL # Reset the inverse when matrix changes
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the inverse
  getInverse <- function() inv
  
  # Return a list of functions
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache rather than recalculating it.
cacheSolve <- function(x, ...) {
  # Try to get the cached inverse
  inv <- x$getInverse()
  
  # If inverse exists in cache, return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If not cached, calculate the inverse
  matrix_data <- x$get()           # Get the original matrix
  inv <- solve(matrix_data, ...)   # Calculate inverse using solve()
  x$setInverse(inv)                # Cache the inverse
  
  # Return the inverse
  inv
}

## Example usage:
## Create a test matrix
# test_matrix <- matrix(c(2, 1, 1, 2), nrow = 2, ncol = 2)
# 
## Create the special matrix object
# cached_matrix <- makeCacheMatrix(test_matrix)
# 
## First call - computes and caches the inverse
# result1 <- cacheSolve(cached_matrix)
# print(result1)
# 
## Second call - retrieves from cache (will show "getting cached data" message)
# result2 <- cacheSolve(cached_matrix)
# print(result2)