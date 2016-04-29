## cachematrix.R
##
## This code was written for the Programming Assigment 2 of the R Programming 
## class on Coursera's Data Science Specialization.
##
## Author: Daniela Petruzalek @ 29-apr-16

## makeCacheMatrix: constructor for a matrix that encapsulates the methods to:
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse matrix
# 4.  get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv_m <- NULL ## inverse matrix
        set <- function(y) {
                x <<- y
                inv_m <<- NULL ## any assignment resets previous inverse calculation
        }
        get <- function() x
        setinverse <- function(inverse) inv_m <<- inverse
        getinverse <- function() inv_m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)      
}


## cacheSolve: calculates the inverse of a matrix and caches its result for repeated use

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinverse(inv)
        inv
}

## Test case
##
# > m <- makeCacheMatrix(matrix(1:4,2,2))
# > m$getinverse()
# NULL
# > cacheSolve(m)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(m)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > m$get() %*% m$getinverse()
# [,1] [,2]
# [1,]    1    0
# [2,]    0    1
