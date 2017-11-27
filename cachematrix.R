## These functions were written for the Week 3 Programming Assignment of
## Coursera Data Science : R Programming
## The first function is named as 'makeCacheMatrix', wherein it gets 
## a matrix and then it will set value of the matrix. After that, it will
## get the value of the matrix and then it will set the inverse matrix,
## and finally will get the inverse matrix. 


makeCacheMatrix <- function(x = matrix()) {           ##taking matrix as an input
      inv <- NULL                                     ##setting the value of the matrix
      set <- function(y) {                            ##define the 'set' function
          x <<-y                                      ##sets value of the matrix in the parent environment
          inv <<- NULL                                ##resets the value of 'inv' if there's new matrix   
      get <- function() x                             ##gets the value of matrix                                          
      setInverse <- function(inverse) inv <<- inverse ##sets the value of the invertible matrix
      getInverse <- function() inv                    ##gets the value of the invertible matrix
      list(set = set, get = get, setInverse = setInverse,getInverse = getInverse)
      }
}


## This second function is able to compute the inverse of the special "matrix" that
## is returned by the first function 'makeCacheMatrix'.
## If the calculated inverse did not changed, then this function 'cacheSolve' will
## reclaim the inverse from the cache.



cacheSolve <- function(x, ...) {
        invMatrix <- x$getInverse()
        if(!is.null(invMatrix)) {                  ##if inverse matrix is not null
                  message("getting cached data")   ##There should be a message: "getting cached data"
                  return(invMatrix)                ##reclaim/return the invertible matrix
        }     
        dataMatrix <- x$get                       ##get the original data matrix
        invMatrix <- solve(dataMatrix,...)        ##this will inverse the matrix using 'solve' function
        x$setInverse(invMatrix)                   ##this will set the invertible matrix
        invMatrix                                 ##reclaim/return the invertible matrix
}


