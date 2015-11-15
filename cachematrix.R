## This assignment let us demonstrate the use of R's scoping facilities in order
## to create "special" list object which wraps a matrix object in order to 
## compute the inverse of the matrix and to keep a cached copy of the operation.
## This allows us to compute the inverse of a given matrix just once, and to
## return the cached result when we ask the computation of the inverse for that
## matrix a second time, saving time which would be used to calculate the
## inverse each time we ask for it.

## The following function creates the "special" list which wraps a given matrix.
## It contains a series of functions which allows us to set the matrix, to 
## retrieve it, to set its inverse result in memory and to retrieve it.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The following function takes the the "list wrapper" object produced by the
## "makeCacheMatrix" function above as an argument. It then checks if the list
## object already contains the (cached) result for the inverse computation of 
## the matrix it holds inside itself, in which cases it returns this cached
## result. If the provided list object does not contain the cached result of the
## inverse of the matrix, the function calculates the inverse using the 
## "solve()" function and it stores the result inside the list object which
## wraps the matrix as a cached copy: in this way, the next time we request the
## inverse of the matrix contained in that same list object, this function just
## retrieve the cached result and returns it.

cacheSolve <- function(x, ...) {
    # Let's check if the provided "x" list object already contains the cached 
    # result for the inverse operation on the matrix contained in the object
    inv <- x$getinv() 
    # If the object does contain the pre-computed, cached result for the inverse
    # of the matrix, return it.
    if(!is.null(inv)) {
        message("Retrieving cached data...")
        return(inv)
    }
    # Otherwise, if there is no cached result for the inverse matrix operation
    # (it is NULL), then retrieve the matrix contained in the provided list 
    # wrapper, compute its inverse, cache it in memory for later use and return
    # the result.
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
