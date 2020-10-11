## This function calculates the inverse of a matrix and saves it
## If the inverse has been calculated before it finds it from the cache, instead of calculating again

## This function calculates the inverse of a matrix and caches it

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){
                x <<- y
                inverse <<- NULL}
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        setinverse <- function() inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)}


## This function returns the inverse of a matrix
## If the inverse was calculated before, this function finds it from the cache

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)}
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
