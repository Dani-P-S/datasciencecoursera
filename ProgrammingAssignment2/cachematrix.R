## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix incloudes 4 functions: 
## set - assign the matrix
## get - get the matrix
## setmatrix - solve the matrix
## getmatrix - get the solved matrix  

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## This function calculates the inverse of the a matrix defined in the previous function. If the inverse value is already calculated, the function returns this value, if not the function solves the matrix and then return the value.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
