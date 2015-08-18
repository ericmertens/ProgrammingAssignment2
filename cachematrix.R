## Author : Eric Mertens

## This file contains 2 functions : makeCacheMatrix and cacheSolve
## Those 2 functions are coming from the example given for the Programming Assignement 2

## The function makeCacheMatrix is an adaptation of the makeVector given as example 
## for the Programming Assignment 2
## "m" (for mean) has been renamed to "im" (for inverted matrix)
## "mean" has been replace by "solve"
## "setmean" and "getmean" have been renamed to "setinvmat" and "getinvmat"


makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setinvmat <- function(solve) im <<- solve
        getinvmat <- function() im
        list(set = set, get = get,
             setinvmat = setinvmat,
             getinvmat = getinvmat)
}

## The function cacheSolve is an adaptation of the cacheMean given as example 
## for the Programming Assignment 2
## "m" (for mean) has been renamed to "im" (for inverted matrix)
## "mean" has been replace by "solve"
## "setmean" and "getmean" have been renamed to "setinvmat" and "getinvmat"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getinvmat()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinvmat(im)
        im
}

## Use the following code as test if needed:
## c <- rbind(c(1, -1/4), c(-1/4, 1))
## cm <- makeCacheMatrix(c)
## c %*% cacheSolve(cm)
## the last line should return an identity matrix (2X2)
##     [,1] [,2]
##[1,]    1    0
##[2,]    0    1
##
