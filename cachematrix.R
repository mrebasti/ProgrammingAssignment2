## This script contains two functions, makeCacheMatrix and cacheSolve, that can
## be used in conjunction to avoid calling the function 'solve' more than once
## for the same matrix. This is achieved by implementing a caching algorithm.

#' This function creates a special "matrix" object that can cache its inverse.
#' 
#' @param data_content A matrix.
#' @return A list containing \code{data_content} and the functions:
#'         \code{set}
#'         \code{get}
#'         \code{setinverse}
#'         \code{getinverse}
#' @examples
#' cachematrix <- makeCacheMatrix()
#' cachematrix <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE))
makeCacheMatrix <- function(data_content = matrix()) {
    # This object will contains the cached result
    inverse <- NULL
    
    # Function that set data_content and clean the previous cached results
    set <- function(x) {
        data_content <<- x
        inverse <<- NULL
    }
    
    # This function returns data_content
    get <- function() data_content
    
    # This function set the cached result in inverse
    setinverse <- function(i) inverse <<- i
    
    # This function retrieves the cached result stored in inverse
    getinverse <- function() inverse
    
    # Returns a list with all the functions created in makeCacheMatrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


#' This function computes the inverse of the special "matrix" returned by
#' \code{makeCacheMatrix} above. If the inverse has already been calculated
#' (and the matrix has not changed), then \code{cacheSolve} should retrieve the
#' inverse from the cache.
#' 
#' @param x A list produced by \code{makeCacheMatrix}.
#' @param ... Extra params to be used by \code{solve}.
#' @return The inverse of the matrix contained in \code{x$get()}
#' @examples
#' inverse <- cacheSolve(makeCacheMatrix())
#' 
#' cachematrix <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE))
#' inverse <- cacheSolve(cachematrix)
cacheSolve <- function(x, ...) {
    # Trying to get the previous result
    inverse <- x$getinverse()
    
    if(!is.null(inverse)) {
        # If there was a previous result cached, the function returns its value
        message("getting cached data")
        return(inverse)
    }
    
    # If there wasn't a previous result cached, the function tries to calculate
    # the inverse of the matrix
    
    data <- x$get() # Getting the matrix data
    inverse <- solve(data, ...) # Calculating the matrix inverse
    x$setinverse(inverse) # Caching the new result in x
    
    inverse # Returning the inverse of the matrix
}
