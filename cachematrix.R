##changes by csmith 2/17 added comments

## makeCacheMartix is a function that 
## allows you to store a matrix and its inverse 
##and returns to you a list of functions that allow you to 
##get and set these values. the input matrix is stored in the variable
## x and the inverse is stored in the variable inverse_cache
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


## the cache solve function takes an input variable that is 
## the list returned by makeCacheMatrix. It then checks to see if
## the matrix contain the value of inverse if so it returns this
## There is comment printed saying getting cached data
##Otherwise it computes the inverse using the solve function, stores it in 
## the cache and return the cpmputed inverse. Typically the second time onwards 
##you will access the cached value.
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

##sample test code
##t_matrix=matrix(seq(1:4),2)
##mcache=makeCacheMatrix(t_matrix)
##cacheSolve(mcache)
##cacheSolve(mcache)



