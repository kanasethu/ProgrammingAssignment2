## This file contains two functions:
##    makeCacheMatrix() - a special function that accepts as its input an
##                        invertible matrix and returns a list of functions
##                        that effectively allows the user to cache a
##                        matrix and its inverse.
##
##    cacheSolve() - a function that accepts as it input the output of
##                   makeCacheMatrix(), which stores the value of a matrix
##                   and possibly its inverse.  If the inverse of the matrix
##                   is cached then the cached value is returned else the
##                   inverse is calculated and stored in cache.
##
## Example Usage:
##    > a <- matrix(1:4, 2, 2)
##
##    > ma <- makeCacheMatrix(a)
##
##    > cacheSolve(ma)
##         [,1] [,2]
##    [1,]   -2  1.5
##    [2,]    1 -0.5
##
##    > cacheSolve(ma)
##    getting cached data
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
##    getting cached data
##         [,1] [,2]
##    [1,]   -4  3.5
##    [2,]    3 -2.5

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # Initialize cached value of matrix inverse
        i <- NULL

        # Function to set the value of a new matrix
        set <- function(y) {
                x <<- y
                # Since we are setting the value of the matrix,
                # void the cached inverse of the previous matrix.
                i <<- NULL
        }

        # Function to return the stored matrix
        get <- function() x

        # Function to the set inverse of the matrix
        setinverse <- function(inverse) i <<- inverse

        # Function to the get inverse of the matrix
        getinverse <- function() i

        # Return the list of four functions defined above
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
                 # If the cached value is not NULL return it
                 message("getting cached data")
                 return(i)
        }

        # If the cached value was NULL, calculate the inverse
        data <- x$get()
        i <- solve(data, ...)

        # Store/Cache the newly caclulated value
        x$setinverse(i)

        # Return the inverse
        i
}
