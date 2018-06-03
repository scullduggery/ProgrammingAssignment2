## The following functions work together to cache and then retrieve the inverse of a Matrix

## Create function which Cache's the inverse of a Matrix
## makeCacheMatrix creates a list of functions which:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the inverse of the matrix to the parent environment
## 4. Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the Cached Inverse of the Matrix 
## or if the Cached inverse is null then it calculates the inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

##Testing out the Functions

aMatrix <- makeCacheMatrix(matrix(c(5, 1, 0,
                             3,-1, 2,
                             4, 0,-1), nrow=3, byrow=TRUE))

cacheSolve(aMatrix)

## Returns:
## [,1]    [,2]   [,3]
## [1,] 0.0625  0.0625  0.125
## [2,] 0.6875 -0.3125 -0.625
## [3,] 0.2500  0.2500 -0.500

## Rerun 

cacheSolve(aMatrix)

## Returns:
## getting cached data
## [,1]    [,2]   [,3]
## [1,] 0.0625  0.0625  0.125
## [2,] 0.6875 -0.3125 -0.625
## [3,] 0.2500  0.2500 -0.500
