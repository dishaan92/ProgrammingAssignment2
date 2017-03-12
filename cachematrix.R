## The following two functions are for creating a object that can store a 
## matrix and then cache's its inverse

## makeCacheMstrix can create a matrix that contains a list of functions doing
## following jobs:
## set the value of the matrix; get the value of the matrix; set the value
## of the inverse matrix; get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse_m <- NULL
    # The following are the four functions: set, get, setinverse, getinverse
    set <- function(y){
        x <<- y
        inverse_m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve function is for getting the inverse of the matrix which we created
## in the makeCacheMatrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse_m <- x$getinverse()
    if(!is.null(inverse_m)){
        message("getting cached matrix")
        return(inverse_m)
    }
    matrix_data <- x$get()
    inverse_m <- solve(matrix_data, ...)
    x$setinverse(inverse_m)
    inverse_m
}
