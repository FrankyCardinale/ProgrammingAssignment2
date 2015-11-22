## Programming Assignment 2 - R Programming
## Data Science Specialization Track
##
## These two functions are part of completing the second
## programming assignment for the R Programming course 
## offered on Coursera through the Johns Hopkins
## School
##
## The two functions shown here help in caching the
## inverse of a matrix.  Matrix inversion is usually
## very computationally intensive - especially for large
## size matrices.  Sometimes in code (and especially in loops),
## the inverse of a matrix need only be computed once.  
## To avoid recomputing the inverse and generating the
## same result repeatedly, we can simply compute the
## result once.  If you try to recompute the inverse again,
## we have already computed this already and so we should
## just return this pre-computed result.
##

## makeCacheMatrix:
## The following function makeCacheMatrix creates a special “matrix”, which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix
##
## Usage example (matrix 4x4 with random integer numbers):
## > x <- matrix(sample(100,16,T),4)
## > m <- makeCacheMatrix(x)
## Print matrix
## > m$get()

makeCacheMatrix <- function(x = matrix()) {
  # Initially the inv is set to NULL
  inv <- NULL
  
  # set the matrix itself but not the inverse  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get the matrix itself but not the inverse  
  get <- function() x
  
  # set the inverse  
  setinverse <- function(inverse) inv <<- inverse
  
  # Get the inverse  
  getinverse <- function() inv
  
  # Encapsulate into a list  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve:
## The following function cacheSolve calculates the inverse of the 
## special “matrix” created with the above function it first checks 
## to see if the inverse of the matrix has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the inverse
## matrix in the cache via the setinverse function.
##
## Usage example (with the matrix 'm' previously created):
## Print the inverse matrix and cache the result
## > cacheSolve(m)
## Print the inverse matrix stored in cache
## > cacheSolve(m)
## getting cached matrix ...

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # Get the current state of the inverse and see if it
  # has been calulated yet  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  
  # Get the matrix itself  
  matrix <- x$get()
  
  # Calculate the inverse  
  inv <- solve(matrix, ...)
  
  # Cache the inverse matrix in the object  
  x$setinverse(inv)
  
  # Return this inverse matrix  
  inv
}
