## This is a pair of functions that compute and cache the inverse of a matrix

## The first function creates a special "matrix" object that caches its inverse,
## which is a list of four functions to set/get the value of the matrix, and to 
## set/get the value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set =set, get = get, setinv = setinv, getinv = getinv)
}

## The second function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached inverse of the matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
