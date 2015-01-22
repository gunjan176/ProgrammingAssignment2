## Matrix inversion is usually a costly computation and their may be some benefit to caching the 
## inverse of a matrix rather than compute it repeatedly. 
## These pair of functions mentioned below cache the inverse of a matrix
##        1. makeCacheMatrix :  creates a special "matrix" object that can cache its inverse
##        2. cacheSolve :  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.


## This function creates a special "matrix" object that 
## can cache its inverse.
## 
## @param   of type matrix
## 
## @return  A list which stores the matrix and its inverse 
##          and contains four functions to set and get the
##          value of matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        # set new value of matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # get current value of matrix whose inverse may have ben cached
        get <- function() x
        # set the inverse of the matrix
        setinv <- function(inverse) inv <<- inverse
        # get the inverse of the matrix
        getinv <- function() inv
        # set a list element in the list
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


##      This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##      If the inverse has already been calculated (and the matrix has not changed), 
##      then the cachesolve should retrieve the inverse from the cache.

## @param   of type special "matrix" as returned by the function makeCacheMatrix
## 
## @return  Either the cached or the computed inverse

cacheSolve <- function(x, ...) {
        #get the inverse
        inv <- x$getinv()
        #check if the inverse is not null
        if(!is.null(inv)) {
                message("getting cached data")
                #if the inverse is not null return the inverse value
                return(inv)
        }
        
        matrix <- x$get() #get the current value of matrix
        
        inv <- solve(matrix,...) #calculate inverse
        x$setinv(inv)  #set the inverse for later use
        inv #return inverse 
}
