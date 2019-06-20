## This program is a set of R functions that are able to store the matrix and 
## also to cache the inverse of a matrix.

## The first function, makeCacheMatrix creates a special "matrix" object,
## that can cache its inverse.

##      It can:
##      a:set the value of the matrix.
##      b:get the value of the matrix.
##      c:set the value of the inverse of the matrix.
##      d:get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() {
                x
        }
        set_inverse <- function(my_inverse) {
                inverse <<- my_inverse
        }
        get_inverse <- function() {
                inverse
        }
        list(set = set, 
             get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)

}

## The second function cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve retrieves the inverse from
## the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$get_inverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$set_inverse(inverse)
        inverse
}
