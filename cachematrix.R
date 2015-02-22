## 
## The below R functions will provide an example of how R can be used to 
## cache potentially time consuming computations.  For instance for a very long vector,
## a value for a mean may take longer when computed repeatly in a loop.  It would be 
## less time consuming to cache the value of the value of the mean and lookup in cache (R Programming). 
## 
## Reference: R Programming, R Programming - John Hopikins Bloomberg School of Public Health
## https://class.coursera.org/rprog-011.
##

# Programming Assignment 2 (Part 1) - R Programming 
# 
# Function "makeCacheMatrix" creates a special matrix that can cache its inverse.
# The function will:
#         - set the value of the matrix
#         - get the value of the matrix
#         - set the inverse of the matrix
#         - get the inverse of the matrix
# 
# Reference:  Coursera - John Hopikins Bloomberg School of Public Health,
# R Programming.
#
makeCacheMatrix <- function(x = matrix()) {
    #  
    # Set the matrix "m"intially to NUll
    m <- NULL
    set <- function(y) {
      #
      # Assign the <<- operator which is used to assign the value 'y'
      # to an evironment that is different from the current environment.
            x <<- y
            m <<- NULL
    }
    
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    #
    # list the functions.
    #
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# 
# Function "cacheSolve" computes the inverse of the special matrix created
# by the function "makeCacheMatrix".  It will first check if the inverse
# was calculated. If the inverse has been calculated the function will get 
# the inverse from the cache.  If not, the inverse of the data is calculated 
# 
# Reference:  Coursera - John Hopikins Bloomberg School of Public Health,
# R Programming.
#
cacheSolve <- function(x, ...) {
  # 
  m <- x$getinv()
  
  #
  # Check if inverse has been calculated.
  #
  if(!is.null(m)) {
      message("getting cached data")
      return(m)
  }
  
  # create the matrix, as it does not exist.
  data <- x$get()
  
  # return the inverse of the function.
  m <- solve(data, ...)
  
  # Set the matrix and display the it. 
  x$setinv(m)
  return(m)
}

