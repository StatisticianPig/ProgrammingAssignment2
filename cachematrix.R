## Note, these functions were easily adapted from the makeVector and
# cachemean functions provided as examples for the assignment, so their 
# functionality is very similar. I.e. they create a special object in R that 
# stores the given input matrix and caches its inverse.

## makeCacheMatrix should create a special "matrix" object that can cache
## its associated inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) i <<- inverse
        get_inverse <- function() i
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## cacheSolve, on the other hand, computes the inverse of "matrix" object 
## defined by the first function. Alternatively, if the inverse has already 
## been calculated (and has not changed), then cacheSolve retrieves the inverse 
## stored in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$get_inverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        matrix <- x$get()
        i <- solve(matrix, ...)
        x$set_inverse(i)
        i
}
