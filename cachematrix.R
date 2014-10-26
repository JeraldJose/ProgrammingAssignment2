## creates and caches the inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}

cacheSolve <- function(xx, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- xx$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- xx$get()
    i <- solve(data, ...)
    xx$setinverse(i)
    i
}