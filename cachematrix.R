## cachematrix.R
##
## This code was written for the Programming Assigment 2 of the R Programming 
## class on Coursera's Data Science Specialization.
##
## Author: Daniela Petruzalek @ 29-apr-16

## makeCacheMatrix: 
#  This function creates a special type of matrix that can cache its inverse.
#  It encapsulates the methods to:
#
# 1.  set the value of the matrix (set)
# 2.  get the value of the matrix (get)
# 3.  set the value of the inverse matrix (setinverse)
# 4.  get the value of the inverse matrix (getinverse)

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

## cacheSolve: this function computes the inverse of the special matrix returned by 
#  makeCacheMatrix. If it finds that the value is already in cache, it returns the cache. 
#  If not, it will compute the inverse using solve(...) and store the value for future use.
#
#  Note: this function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...) ## expensive computation!
        x$setinverse(inv)
        inv
}

## Sample test case
#
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
