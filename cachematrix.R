# RA-1
# makeCacheMatrix function is generating a list object that overwraps a genuine matrix object
# 4 methods are available
# set(y)          : this method is called when the genuine matrix have to be replaced. the inverse is reset to null
# get()           : this method is called by the cacheSolve function in order to retrieve the genuine matrix (from the function enrironment) 
#                   in order to compute its inverse
# setsolve(solve) : this method is called in order to store the inversed cached matrix. (function environment)
# getsolve()      : this method is called by the cacheSolve function in order to retrieve the inversed cached matrix
#
# usage:
#
# m <- matrix(rbinom(1:100,10,.5),10,10)
# cm <- makeCacheMatrix(m)
# cacheSolve(cm)
#              [,1]        [,2]        [,3]         [,4]        [,5]        [,6]   ...
# [1,] -0.014200647  0.07495445  0.09100149 -0.148645798 -0.02280078  0.02728514
# ...
#
# cacheSolve(cm)
# getting cached data
#              [,1]        [,2]        [,3]         [,4]        [,5]        [,6]   ...
# [1,] -0.014200647  0.07495445  0.09100149 -0.148645798 -0.02280078  0.02728514
# ...




makeCacheMatrix <- function(x = matrix()) {
        s <- NULL                                # initialisation of inverse to null
        set <- function(y) {
                x <<- y                          # replace the genuine matrix
                s <<- NULL                       # reset the inverse to null
        }
        get <- function() x                      # retrieve the genuine matrix function
        setsolve <- function(solve) s <<- solve  # cache the solve matrix function. solve is a variable in this case not a function
        getsolve <- function() s                 # retrieve the inversed cached matrix function
        list(set = set, get = get,               # list object returned
             setsolve = setsolve,
             getsolve = getsolve)
}

# cacheSolve function is used in order to retrieve inverse of the genuine matrix encapsulated inside the makeCacheMatrix
# the first argument must be a list generated with makeCacheMatrix
# The other arguments must be in accordance with the arguments of the matrix solve method.
# when cacheSolve is called, the function first checks if the solve method was already called and if it is the case ( s not null ) then
# the inversed cached matrix is returned. Otherwise the inversed matrix is computed, cached and returned

cacheSolve <- function(x, ...) {
        s <- x$getsolve()                # get the inversed cached matrix
        if(!is.null(s)) {                # if inverse is not null
                message("getting cached data")
                return(s)                # return the cache value
        }
        data <- x$get()                  # get the genuine matrix
        s <- solve(data, ...)            # compute the inverse
        x$setsolve(s)                    # cache the inversed matrix
        s                                # return the inversed matrix
}


