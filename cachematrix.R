## ----------------------------------------------------------------------------------
## Special matrix class which also stores its inverse value
## ----------------------------------------------------------------------------------
## 
## To create cacheMatrix object: cache_matrix <- makeCacheMatrix(matrix)
## To compute inverse and store it: cacheSolve(cache_matrix)
## To access matrix and inverse: cache_matrix$get() and cache_matrix$get_inverse()
## ----------------------------------------------------------------------------------

##This function creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(matrix_data = matrix()) {
    inverse <- NULL
    
    ## set function to store matrix data
    set <- function(y) {
        matrix_data <<- y
        inverse <<- NULL
    }
    
    ## return matrix data
    get <- function() matrix_data
    
    ## compute inverse of matrix and store in inverse
    set_inverse <- function(solve) inverse <<- solve
    
    ## return inverse of matrix
    get_inverse <- function() inverse
    
    ## list all available functions
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}

## This function computes the inverse of the special matrix returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(cmatrix, ...) {
    ## get inverse from matrix if already computed
    inverse <- cmatrix$get_inverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    ## otherwise compute inverse and store it
    data <- cmatrix$get()
    inverse <- solve(data, ...)
    cmatrix$set_inverse(inverse)

    ## Return a matrix that is the inverse of cmatrix    
    inverse
}