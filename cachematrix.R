## This script file contains two functions:
##
## makeCacheMatrix(): makes object to store and retrieve a square and invertable 
##                    matrix and it's inverse in cache
## cacheSolve():      returns inverse of a matrix, by retrieving it from cache 
##                    (if available) or by calculating using solve() otherwise.


####  makeCacheMatrix  ####
##
## Creates a matrix-like object on which four functions are defined to
## store and retrieve the matrix x and its inverse in/from the cache.
## When called without argument, an emtpy object is made
## 
## Input: x, a square and invertible matrix
##
## Remarks:
## * No check on invertability of input matrix x.
## * When M has been created with makeCacheMatrix:
##   * At calling M$set(y): no check if y has inverse, M$getInv() is set to NULL
##   * Checks whether Minv is inverse to 'M' if the inverse is set by calling
##     M$set_inv(Minv). 
##   

makeCacheMatrix <- function(x = matrix()) {
        x_inv <- NULL
        set <- function(y){
                x <<- y
                x_inv <<- NULL
        }
        get <-function() x
        setInv <- function(inv) {
                I <- diag(nrow(x))
                # to prevent error messages of %*%: first check if inv is matrix
                # of right dimension before check if it is inverse of x 
                if ((class(inv) == "matrix") && (ncol(x) == nrow(inv)) && (identical(x %*% inv,I))){
                        x_inv <<- inv
                }
                else {
                        message(" No inverse set in cache, argument is no inverse to matrix ..$get()")
                }
        }        
        getInv <- function() x_inv
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


####  cacheSolve  ####
## 
## Returns the inverse of a 'matrix' x. The inverse is retrieved from the cache 
## unless no valid inverse of x is stored there, then the inverse is calculated 
## and set to the cache. 
##
## Input: x, a matrix-like object created with makeCacheMatrix
##        ... arguments to/from called functions


cacheSolve <- function(x, ...) {
        x_inv <- x$getInv()
        x_mat <- x$get()        # x_mat is matrix             
        I <- diag(nrow(x_mat))
        if (!is.null(x_inv)) {
                       message("inverse from cached data")
                return(x_inv)
        }
        x_inv <- solve(x_mat)
        x$setInv(x_inv)
        x_inv
}