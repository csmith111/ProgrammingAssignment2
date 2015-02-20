
# Github project location
# https://github.com/csmith111/ProgrammingAssignment2.git
# 
# makeCacheMartix is a function that 
# allows you to create a structure that holds a matrix 
# and its inverse along with list of functions that allow you to 
# get and set the matrix and its inverse. 
# the input matrix is stored in the variable x 
# and the inverse is stored in the variable inverse_cache

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


# the cacheSolve function takes an input variable that is 
# the structure returned by makeCacheMatrix. It then checks to see if
# the matrix contains the value of inverse 
#       if TRUE it returns this value and prints a comment saying getting cached data
#       if FALSE it computes the inverse using the solve function, stores it in 
#        the cache and return the cpmputed inverse. 
#Typically the second time onwards  you will access the cached value.

cacheSolve <- function(x, ...) {
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

##test code
runTest<-function(){
        t_matrix=matrix(seq(1:4),2)
        mcache=makeCacheMatrix(t_matrix)
        #the first call will require the computation to be done
        print("first invocation: ")
        print(cacheSolve(mcache))
        
        #The second call - the cache will be used 
        print("second invocation: ")
        print(cacheSolve(mcache))
}


