## Overall, the first function, makeCacheMatrix creates a cache in which to store the values of the matrix and its inversion.  The second function, cacheSolve, takes a matrix, x, checks to see if there is already a value stored away..  If so, it grabs the value and returns it.  If not, it solves the inversion, stores the value in the cache, and then returns the value.

## This function, makeCacheMatrix, creates a special "vector", which is really a list containing a function to 1)set the value of the matrix 2)get the value of the matrix 3)set the value of its inverse 4)get the value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    thelist <<- list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function, cacheSolve, calculates the inverse of the special "matrix". It first checks to see if the inverse has already been calculated.  If so, it gets the inverse cache and skips the computation.  Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- thelist$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- thelist$get()
    m <- solve(data, ...)
    thelist$setinverse(m)
    m
}
