## John R. Duong
## R Programming - Assignment 2
## Coursera - Johns Hopkins University
## August 23, 2014
##
##
## Description:
## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly. The following pair of functions cache the inverse of 
## a matrix.
##
##
## Sample Run:
##> j <- matrix(1:4, 2, 2)
##> k<-makeCacheMatrix(j)
##> k$getMInv()
##NULL
##
##> cacheSolve(k)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##
##> k$getMInv()
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##
##> cacheSolve(k)
##getting cached data
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##
##> k$set(matrix(3:6, 2, 2))
##> k$getMInv()
##NULL
##
##> cacheSolve(k)
##[,1] [,2]
##[1,]   -3  2.5
##[2,]    2 -1.5
##
##> cacheSolve(k)
##getting cached data
##[,1] [,2]
##[1,]   -3  2.5
##[2,]    2 -1.5
##
##> k$get()
##[,1] [,2]
##[1,]    3    5
##[2,]    4    6
##
##
## Functions:
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setMInv <- function(minv) m <<- minv
        getMInv <- function() m
        list(set = set, get = get,
             setMInv = setMInv,
             getMInv = getMInv)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above.  If the inverse has already been calculated (and
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
        m <- x$getMInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setMInv(m)
        m
}
