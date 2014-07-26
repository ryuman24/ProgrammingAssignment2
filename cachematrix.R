## Function computes the inverse of a matrix, and stores
## that inverted matrix for future use. If no matrix has
## been computed when first called, function will compute,
## store and return inverted matrix.

## Uses the solve function instead of the mean function
## to compute the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



## Write a short comment describing this function
## When called, function returns the already computed
## inverted Matrix. If not already computed, setInverse
## is used to compute, and then return inverted matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("Getting cached data...")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
