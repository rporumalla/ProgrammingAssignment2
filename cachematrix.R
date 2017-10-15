## Put comments here that give an overall description of what your
## functions do

## "makeCacheMatrix" function creates a special "matrix" object that
## can caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## "cacheSolve" function computes the inverse of the special matrix
## returnd by "makeCacheMatrix" above. If the inverse has already been
## calculated (and the matrix has not changed), then "cacheSolve" will
## retrieve the inverse from the cache.
## x is a square invertible matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of matrix in cache
        i <- x$getinv()
        if (!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        data <- x$get()
        if (nrow(data) == ncol(data)) {  # check if it is a square matrix
            i <- solve(data)       # computes the inverse of a square matrix
            x$setinv(i)
            i
        }
        else
            message("Not a Square Matrix")
}
