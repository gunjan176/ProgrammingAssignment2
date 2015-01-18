## Matrix inversion is usually a costly computation and their may be some benefit to caching the 
## inverse of a matrix rather than compute it repeatedly. 
## These pair of functions mentioned below cache the inverse of a matrix
##        1. makeCacheMatrix :  creates a special "matrix" object that can cache its inverse
##        2. cacheSolve :  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.


## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cacheMatrix <- makeCacheMatrix(x)
        inv <- cacheMatrix$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix <- cacheMatrix$get()
        inv <- solve(matrix,...)
        cacheMatrix$setinv(inv)
        inv
		
}
