## This file contains two functions:
##    makeCacheMatrix() - a special function that accepts as it input an
##                        invertible matrix and returns a list of functions
##                        that effectively allows the user to cache a
##                        matrix and its inverse.
##
##    cacheSolve() - a function that accepts as it input the output of
##                   makeCacheMatrix(), which stores the value of a amtrix
##                   and possibly its inverse.  If the inverse of the matrix
##                   is cached then the cached value is returned else the
##                   inverse is calculated and stored in cache.
##
## Example Usage:
##    > a = matrix(1:4, 2, 2)
##
##    > ma <- makeCacheMatrix(a)
##
##    > cacheSolve(ma)
##         [,1] [,2]
##    [1,]   -2  1.5
##    [2,]    1 -0.5
##
##    > cacheSolve(ma)
##    getting cachced data
##         [,1] [,2]
##    [1,]   -2  1.5
##    [2,]    1 -0.5
##
##    > ma$set(matrix(5:8, 2, 2))
##
##    > cacheSolve(ma)
##         [,1] [,2]
##    [1,]   -4  3.5
##    [2,]    3 -2.5
##
##    > cacheSolve(ma)
##    getting cachced data
##         [,1] [,2]
##    [1,]   -4  3.5
##    [2,]    3 -2.5

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then cacheSolve retrieves the inverse from
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)) {
                 message("getting cachced data")
                 return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
