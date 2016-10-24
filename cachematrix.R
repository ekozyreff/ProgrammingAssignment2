## The objective of the pair of functions 'makeCacheMatrix' and 
##'cacheSolve' is to return the inverse of a matrix in an efficient way.
## It will check whether the inverse is already available before
## calculating it.

## The function 'makeCacheMatrix' will create a list of functions that
## will work together with the main function, 'cacheSolve'.

makeCacheMatrix <- function(x = matrix()) {
    ## Assign 'NULL' to 'm'
    m <- NULL
    ## Assign 'y' to 'x' and 'NULL' to 'm'
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## Get the value of the matrix 'x'
    get <- function() x
    ## Set to 'm' the value of the inverse o 'x'
    setinv <- function(inv) m <<- inv
    ## Get the value of inverse of the matrix 'x'
    getinv <- function() m
    ## Create a list with the functions 'set', 'get', 'setinv', 'getinv'
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function will check whether the inverse of a matrix is stored
## in the Cache. If so, it will retrieve it and display it. Otherwise,
## it will calculated it, store it in the Cache and then display it.

cacheSolve <- function(x, ...) {
    ## Get the inverse of 'x' using the function getinv()
    m <- x$getinv()
    ## If the inverse is available, return it and end the function
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## If the inverse is not available, assign the matrix 'x' to 'data'
    data <- x$get()
    ## Compute the inverse of the matrix 'x'
    m <- solve(data)
    ## Use the function 'setinv' to assign the inverse of 'x' to 'm'
    x$setinv(m)
    ## Return 'm', a matrix that is the inverse of 'x'
    m
}
