
# Resubmission for a course re run.
# Assignment 2 : Coursera rprog-013
###############################################

# Put comments here that give an overall description of what your
## functions do
## The functions here implement a simple cache lookup mechanism for storing
## results from computationaly intensive functions/operations.

## Honestly, I don't think this assignment actually achieves any objective
## besides testing a person's ability to copy paste the vector sample that was provided
## or the ability to use github.
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

## Set inv value to null
        m <- NULL 
## define a function that inits the values when the corresponding SET function is called.
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

## defines a get function that returns the matrix held by this
        get <- function() x

## defines a function that sets the result of computationally intensive function to M.
        setinv <- function(inv) m <<- inv

## defines a function that returns the result of the function available against this matrix.
        getinv <- function() m

## defines a list of functions available against this object.
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
## This function checks the result of getInv against the matrix passed,
## if a result is available, the same is returned from the "cache"
## else the result is recalculated and SET on the passed object through setinv()

cacheSolve <- function(x, ...) {
        
## attempt at getting the result from the matrix object
        m <- x$getinv()

## check if value is not null - Implying that the results are available from last operation
        if(!is.null(m)) {
                message("getting cached data")
## return cached operation result.
                return(m)
        }

## the block reaches this point if the results of the computationaly intensive operation are not available
        data <- x$get()
## call the computationally intensive operation
        m <- solve(data,...)

## cache the results of the operation against the passed object
        x$setinv(m)
## return result
        m
}
