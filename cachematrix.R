# InvertMatrix ------------------------------------------------------------

# Function to create a special matrix object with caching
makeCacheMatrix <- function() {
  # Initialize the matrix and inverse variables
  matrix <- NULL
  inverse <- NULL
  
  # Function to set the matrix
  set <- function(mat) {
    matrix <<- mat
    inverse <<- NULL  # Reset the inverse cache when matrix changes
  }
  
  # Function to get the matrix
  get <- function() {
    matrix
  }
  
  # Function to set the inverse of the matrix
  setInverse <- function(inv) {
    inverse <<- inv
  }
  
  # Function to get the inverse of the matrix
  getInverse <- function() {
    inverse
  }
  
  # Return a list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function to compute and cache the inverse of a matrix
cacheSolve <- function(cacheMatrix) {
  # Check if the inverse is already cached
  if (!is.null(cacheMatrix$getInverse())) {
    message("Retrieving cached inverse")
    return(cacheMatrix$getInverse())
  }
  
  # Compute the inverse
  matrix <- cacheMatrix$get()
  inv <- solve(matrix)
  
  # Cache the inverse
  cacheMatrix$setInverse(inv)
  
  # Return the inverse
  inv
}


# Create a matrix and cache its inverse
myMatrix <- makeCacheMatrix()
myMatrix$set(matrix(c(1, 2, 3, 4), nrow = 2))

# Compute the inverse (first time, computes and caches)
cacheSolve(myMatrix)

# Retrieve the cached inverse (second time, retrieves from cache)
cacheSolve(myMatrix)



