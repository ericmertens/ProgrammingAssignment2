## Author : Eric Mertens
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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

## Write a short comment describing this function

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

## Use c=rbind(c(1, -1/4), c(-1/4, 1)) as test matrix
