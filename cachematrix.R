## These two functions create a special type of matrix that can cache the original
## matrix and its inverse.
## If the inverse has been calculated, it can be retrieved from cache. 
## Otherwise, the inverse will be calculated and then cached.


## return a special matrix object that can cache its inverse
## getMatrix and setMatrix store and set the original matrix object
## getInverse and setInverse get and set the inverse of the original matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(original) {
        x <<- original
        inverse <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inv) {
        inverse <<- inv
    }
    
    getInverse <- function() inverse
    
    list(set = set, get = get, 
         setInverse = setInverse, getInverse = getInverse)

}
## compute the inverse of special matrix object returned by makeCacheMatrix above
## if the inverse has already been calculated (and the matrix hasn't changed)
## then cacheSolve should just retrieve the cached matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    message("no cached data available")
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$setInverse(inverse)
    inverse
}
