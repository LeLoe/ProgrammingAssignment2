

## Matrix inversion is usually a costly computation and their may be some 
##benefit to caching the inverse of a matrix rather than compute it repeatedly
## These pair of functions are created to cache the inverse of a matrix.
## The inverse of the matrix is calculated once and cached and can be looked up
## rather than recomputed.


##The first function, `makeMatrix` creates a special "matrix", which is
##really a list containing a function to

##1.  set the value of the matrix
##2.  get the value of the matrix
##3.  set the value of the inverse of the matrix
##4.  get the value of the inverse of the matrix

 makeCacheMatrix <- function(x = matrix()) {
       inverse <- NULL
       set <- function(y) {
             x <<- y
             inverse <<- NULL
         }
       get <- function() x
       setinverse <- function(inv) inverse <<- inv
       getinverse <- function() inverse
       list(set = set, get = get,
                       setinverse = setinverse,
                      getinverse = getinverse)
   }



## The following function calculates the inverse of the special "matrix"
##created with the above function. However, it first checks to see if the
##inverse has already been calculated. If so, it `get`s the inverse from the
##cache and skips the computation. Otherwise, it calculates the inverse of
##the data and sets the value of the inverse in the cache via the `setinverse`
##function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inverse <- x$getinverse()
         if(!is.null(inverse)) {
            message("getting cached data")
           return(inverse)
         }
         data <- x$get()
         inverse <- solve(data, ...)
         x$setinverse(inverse)
         inverse
}
