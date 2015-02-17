##changes by csmith 2/16

## the makeCache martix creates function that 
## allows you to set/get a vector and its inverse 
makeCacheMatrix <- function(x = matrix()) {
        inverse_cache <- NULL
        set <-function(y){
                x<<-y
                inverse_cache <<- NULL
        }
        get <- function() x
        setinverse <-function(inverse) inverse_cache <<-inverse
        getinverse <- function() inverse_cache
        list(set =set, get=get, setinverse = setinverse, getinverse = getinverse)
}


## the makeCache martix creates function that 
## allows you to set/get a vector and its inverse
## This function caches the result of computing the inverse
## and reuses this on future calls

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
