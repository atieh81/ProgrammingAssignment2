# Caching the Inverse of a Matrix
# To use these function, you should call the first function "makeCacheMatrix" and then send its 
# result to the secod function to get the inverse matrix.

# This function gets a Matrix as an input and creates a special "List" of functions.
# It can cache and return the inverse matrix of the input.
makeCacheMatrix <- function(x = matrix()) {

  c <- NULL

  # This Method sets the value of the matrix
  setMat <- function(y) {
    x <<- y
    c <<- NULL
  }

  # Returns the value of the matrix
  getMat <- function() x
  
  # Set Inverse matrix
  setCache <- function(z) c <<- z
  
  # Return the Inverse matrix from catche
  getCache <- function() c

  # This function (Class) return a list of methods. So you can call a method from the list
  list(setMat = setMat, getMat = getMat, setCache = setCache, getCache = getCache)
  
}


# This function gets as input the special List of makeCacheMatrix above, and return Inverse matrix
# If the inverse has already been calculated, then the it retrieves the inverse from the cache.
# Otherwise, it calculates the inverse of the matrix and sets the value in the cache via the setCache function.
cacheSolve <- function(x) {

  # Return the Inverse from the cache
  s <- x$getCache()

  # If the value was stored in cache, return it
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }

# Otherwise...
  
  # Get the matrix that was stored by the Set function
  data <- x$getMat()
  
  # Calculate the Inverse
  s <- solve(data)
  
  # Store the Inverse in the cache
  x$setCache(s)
  
  # Return inverse matrix
  s
  
}
