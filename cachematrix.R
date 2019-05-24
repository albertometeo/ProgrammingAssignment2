## Functions for storing a matrix with a cache of its inverse
## This is intended to reduce calculation time when the inverse of a matrix is accessed multiple times


## Function to create a matrix with the possibility of caching its inverse
makeCacheMatrix <- function(x = matrix())
{
    inverse_matrix <- NULL
    set <- function(y)
    {
        x <<-y
        inverse_matrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inv)
    {
        inverse_matrix <<- inv
    }
    getinverse <- function()
    {
        inverse_matrix
    }
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Function to access the inverse of a matrix created with the function makeCacheMatrix
## With this function the inverse will be calculated one time at most, after that it will return the cached result
cacheSolve <- function(x, ...)
{
    inv <- x$getinverse()
    if ( !is.null(inv) )
    {
        message("getting cached data")
        return (inv)
    }
    inv <- solve(x$get(), ...)
    x$setinverse(inv)
    inv
}
