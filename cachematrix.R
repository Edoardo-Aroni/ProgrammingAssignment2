

## The below functions exploit the benefits to cache the inverse of a matrix 
## instead of calculate it repeatedly. 
## The overall logic is based on the assumption that the matrix supplied is
## always reversible. 

## The makeCacheMatrix creates a special matrix object which is a list 
## containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverted matrix
## get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinvertedmatrix <- function(solve) inv <<- solve
        getinvertedmatrix <- function() inv
        list(set = set, get = get, 
             setinvertedmatrix = setinvertedmatrix,
             getinvertedmatrix = getinvertedmatrix)
}



## The function cacheSolve returns the inverted matrix of the special matrix 
## provided by the makeCacheMatrix function.
## First it checks if the inverted matrix has already been calculated and the supplied matrix 
## it is the same. If this is true returns the inverted matrix from the cache.
## If it is false computes the inverted matrix.


cacheSolve <- function(x, ...) {
        inv <- x$getinvertedmatrix()
        if(!is.null(inv)) {   ## check if the matrix has already been computed
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinvertedmatrix(inv)
        inv        ## Return a matrix that is the inverse of 'x'
}
