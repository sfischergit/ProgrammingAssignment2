## These functions create an environment that can cache a matrix inverse,
## and provides a utility to check if that cache is populated, and if not
## determine the inverse and then cache the result


## Function creates a cacheMatrix object with the ability to cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    ##Set up object environment
    inv <- NULL
    ##Set up object behaviors
    get <- function() x
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    setinverse <- function(i) inv <<- i
    getinverse <- function() inv
    
    ##Construct new object
    list(get=get,set=set,setinverse=setinverse,getinverse=getinverse)
}

## Function checks if matrix has cached inverse, if it does not it finds 
## the inverse and caches the result
cacheSolve <- function(x, ...) {
    #Retrieve inverse if cached
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Inverse of matrix was cached")
        return(inv)
    }
    #Inverse not cached so calculate, cache result, and return result
    inv <- solve(x$get())
    x$setinverse(inv)
    inv
}
