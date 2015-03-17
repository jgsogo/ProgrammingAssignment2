## This set of functions allow to cache a matrix and its inverse ('makeCacheMatrix')
## or to compute it in case it is not already cached ('cacheSolve')


## Matrix object that cache its data and inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        # TODO: Check if x == y
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Matrix inverse computation (uses cached data if it's available)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data!")
        return(inv)
    }
    # Compute inverse
    data <- x$get()    
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
