## https://github.com/Nagendra1421/ProgrammingAssignment2.git
## 3e6eb02e2be120d39acc81e3e3cf7c16f06f2360
## The makeCacheMatrix function creates a special "matrix",
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix



makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<-  y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}
## The cacheSolve function calculates the inverse of the special "matrix"
## the special "matrix" which created with the makeCacheMatrix function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse
## in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
