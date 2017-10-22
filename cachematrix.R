# This function, crates a "Cache" object that provides storage for matrix and it's "solve" value,
# additionally, created object provides following get/set functions:
# 
# set(X) - set the value of the matrix
# get(X) - get the value of the matrix
# setsolve(solve) - set the value of the matrix solve
# getsolve(solve) - the value of the matrix solve

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
    
}


## This functions calculates solve() for cache object, created using  makeCacheMatrix function
## calculated solve is saved inside the object
## If solve is alredy calculated, ir is just returned, preliminary without calculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s   
}
