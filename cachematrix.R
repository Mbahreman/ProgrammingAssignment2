# Function to create a special matrix object
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the inverse
  getInverse <- function() inv
  
  # Return list of functions
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# Function to compute the inverse of the matrix
cacheSolve <- function(x, ...) {
  # Get the cached inverse
  inv <- x$getInverse()
  
  # If the inverse is already cached, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If not cached, calculate the inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the inverse
  x$setInverse(inv)
  
  # Return the inverse
  inv
}