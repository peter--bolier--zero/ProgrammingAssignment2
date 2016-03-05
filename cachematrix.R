####
#
# Create matrix with a cached inverse
# We use two functions which togheter implement the caching of the inverse matrix.
#
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve     : Computes the inverse of the special "matrix" returned by makeCacheMatrix if needed.
#
# Usage
# 1 - create a 'special' matrix using makeCacheMatrix
# 2 - Use cacheSolve to computes the inverse of the special "matrix" returned by makeCacheMatrix.
#
# Note: it resembles the object oriented programming paradigm, albeit in a somewhat complicated manner.
#
# Example of combined usage:
# > source("cachematrix.R")
# > mcm <- makeCacheMatrix(matrix(c(1, -1/4,-1/4, 1), nrow = 2, ncol = 2, byrow = TRUE))
# > mcm$getinverse()
# NULL
# > mcm$get()
#       [,1]  [,2]
# [1,]  1.00 -0.25
# [2,] -0.25  1.00
# > 
# > cacheSolve(mcm)
#           [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
# > cacheSolve(mcm)
#getting cached data
#           [,1]      [,2]
#[1,] 1.0666667 0.2666667
#[2,] 0.2666667 1.0666667
# > mcm$get() %*% mcm$getinverse()
#     [,1] [,2]
#[1,]    1    0
#[2,]    0    1
#

####
# Create a special matrix by embedding it in a function.
# 
# Two possiblities
# 1 pass the matrix when calling makeCacheMatrix
# 2 use the set function
#
# $get: get (retrieve) the embedded matrix
# $set: set (stores) a (new) matrix to be used
# $getinverse: get (retrieve) the inverse matrix
# $setinverse: set (stores) the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  x.inverse <- NULL # initially we dont have an inverse
  
  # Get (retrieve) the stored matrix
  # input  : -
  # returns: matrix
  get <- function() {
    x
  }
  
  # Set (store) the matrix
  # input  :matrix
  # returns: -
  set <- function(matrix) {
    x         <<- matrix     # store given matrix
    x.inverse <<- NULL       # New matrix, so reset inverse
  }
  
  # Get the inversematrix
  # input  : -
  # returns: inversematrix
  getinverse <- function() {
    x.inverse
  }
  
  # Set the inversematrix
  # input  : inversematrix
  # returns: -
  setinverse <-function(inverse) {
    x.inverse <<- inverse
  }
  
  list(get = get, set = set, getinverse = getinverse, setinverse = setinverse ) 
}


# Determine inverse of matrix, if possible.
# Inverse is only claculated if needed, otherwise cached inverse is returned.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()      # get cached inverse
  if (!is.null(inv)) {       # check if any value is retrieved
    message("getting cached data")
    return(inv)
  }
  
  # No cached inverse availble, lets calculate it.
  matrix = x$get()           # get original matrix
  inv <- solve(matrix)       # Determine inverse matrix
  x$setinverse(inv)          # Store inverse for reuse
  inv                        # return inverse
}

# note: 
