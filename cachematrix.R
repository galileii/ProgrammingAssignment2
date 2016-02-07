
## This group of functions is used to compute and cache the inverse of a matrix.
## makeCacheMatrix returns a list of functions from which the matrix and its 
## inverse can be set/fetched. 
## cacheSolve uses the cached matrix to solve its inverse (unless already solved)

makeCacheMatrix <- function(x = matrix()) {
# Computes a pseudo vector that can be used to cache and return the inverse matrix
# of x. This function is used in conjunction with cachesolve.
# 
# Args:
#       x: is an invertible matrix
#
# Returns:
#       List with set // get // setinv // getinv
#
# EXAMPLE:
#       d <- cachesolve(makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2)))
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(newinv) inv <<- newinv
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
# Solves the inverse of a function x (unless already computed) and caches the 
# inverted matrix
#
# Args:
#       x: is a list of functions retrieved from makeCacheMatrix
#
# Returns:
#       inv: the inverted matrix
#
# EXAMPLE:
#       d <- cachesolve(makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2)))
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        sizedim <- dim(data)
        if (sizedim[1] != sizedim[2]) {
                message("Not Invertible! The matrix must be square (nxn) for inversion")
                message(sprintf("matrix is %s x %s", sizedim[1], sizedim[2]))
                return(NULL)
        }
        inv <- solve(data)
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
