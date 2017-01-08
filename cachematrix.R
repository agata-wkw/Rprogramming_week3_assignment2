## Two functions that cache and compute the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(M = matrix()) 
{
    M_invers <- NULL
    set <- function(x)
    {
        M <<- x
        M_invers <<- NULL
    }
    
    get <- function() M
    setinv <- function(inv) M_inverse <<- inv
    getinv <- function() M_inverse
    list(set = set, get = get, setinv = setinv, getinv = getinv)
    
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    M_inverse <- M$getinv()
    if(!is.null(inverse)) 
    {
        message("Getting cached data...")
        M_inverse
    }
    data <- M$get()
    M_inverse <- solve(data, ...)
    M$setinv(M_inverse)
    M_inverse
    
}

